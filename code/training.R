library(dplyr)
library(data.table)
library(RWeka)
library(tidyr)

###################
# Environment
#
source("code/config.R")
source("code/constants.R")
source("code/mod.R")

###################
# Implementation
#
#-----------------------------------------------------
# Read sentences from a text file 
#-----------------------------------------------------
# param fileName  : the source text file
# return a data frame of sentences and words count
#
readSample <- function(fileName) {
    cat("Reading ", fileName,"\n")
    df <- tbl_df(read.table(fileName,allowEscapes = T, sep="|", stringsAsFactors = F)) %>%
        rename(text=V1)
    df$text <- sapply(df$text, function(s) sprintf("%s",s))
    Encoding(df$text) <- "UTF-8"
    return(df)
}

#-----------------------------------------------------
# Build a list of n-gram frequency tables
#-----------------------------------------------------
# param sentences   : a character vector
#       ngramOrders : a numeric vector
# return a list of data tables
#
text2ngram <- function(sentences, ngramOrders=c(1)) {
    ngramList <- list()
    for (order in ngramOrders) {
        ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = order, max = order))
        #Build counts
        ngrams <- as.data.frame(table(as.vector(unlist(lapply(sentences, ngramTokenizer)))),
                                stringsAsFactors=F)
        names(ngrams)[1] <- "token"
        
        #Check all same order
        filteredIndices <- which(sapply(ngrams$token,function(s) {length(strsplit(s," ")[[1]])}!=order))
        if (length(filteredIndices) > 0)
            ngrams <- ngrams[-filteredIndices,]
    
        #Re-order
        ngrams <- dplyr::select(ngrams, token, freq=Freq)
        #Split token into words
        colNames <- sapply(seq(1,order), function(s) paste0("word",s))
        ngrams <- tidyr::separate(ngrams,"token",colNames," ")
        #Add to list
        ngramList[[sprintf("%d-gram",order)]] <- data.table(ngrams)
    }
    return(ngramList)
}

#-----------------------------------------------------
# Get marginalized counts
#-----------------------------------------------------
# param dt           : a data table with "word1", 
#                      ... and "freq" columns
#       fixedIndices : indices that are fixed
# return a data table with the fixed columns and
#        their marginalized counts
#
getMarginalizedCount <- function(dt, fixedIndices = c(1), cCount = F) {
    cat("---- Marginalizing on indices", fixedIndices, "\n")
    print(head(dt, n=2))
    #We work on a copy of original table
    if (cCount) {
        cat("  ==> Marginalized counts\n")
        dt$freq <- as.integer(dt$freq>0)
    }
    #Now compute count stat
    index <- NULL; colNames <- NULL
    if (length(fixedIndices) == 0)
        index <- dt[,lapply(.SD,sum),.SDcols="freq"]
    else {
        colNames <- names(dt)[fixedIndices]
        index <- dt[,lapply(.SD,sum), by=colNames,.SDcols="freq"]
        #Index on fixed columns
        setkeyv(index, colNames)
    }
    cat("-----------------------------\n")
    return (list(index=index, colNames=colNames))
}

#-----------------------------------------------------
# Train probabilities for a n-gram order
#-----------------------------------------------------
# param dt       : a data table with "word1", ... 
#                  and "freq" columns
#       discount : a fixed discount
# return a data table with a log10 probability column
#
trainProb <- function(dt, discount=0.75) {
    wIndices <- grep("word*",names(dt))
    order <- length(wIndices)
    cat("###### Normal counts, order", order, "\n")
    fixedIndices = wIndices[-order]
    ret <- getMarginalizedCount(dt, fixedIndices, F)
    setkeyv(dt, ret$colNames)
    dt <-  suppressWarnings(merge(dt, ret$index, all.x=T))
    print(head(dt))
    setnames(dt, "freq.x", "freq")
    setnames(dt, "freq.y", "marginalizedCount")
    dt$logprob <- log10(pmax(dt$freq-discount,0)/dt$marginalizedCount)
    return(dt)
}

#-----------------------------------------------------
# Train continuation probabilities
#-----------------------------------------------------
# param dt       : a data table with "word1", ... 
#                  and "freq" columns
#       discount : a fixed discount
# return a data table with a probability column
#
trainContinuationProb <- function(dtLower, dtHigher, discount=0.75) {
    wIndicesLower <- grep("word*",names(dtLower))
    wIndicesHigher <- grep("word*",names(dtHigher))
    ######################
    # Continuation count:
    # 
    # w1 w2     w1  w2 w3
    # b  c  --> a   b  c
    #       --> d   b  c
    #          ---------
    #           *   b  c
    #
    cat("###### Continuation count:\n")
    fixedIndices = wIndicesHigher[-1]
    ret <- getMarginalizedCount(dtHigher, fixedIndices, T)
    
    cat("  --> Merging lower order column '", names(dtLower)[wIndicesLower], "' with index column '", 
        ret$colNames, "'\nIndex: \n", sep=""); print(ret$index[1,])

    setkeyv(dtLower, names(dtLower)[wIndicesLower])
    dtLower <- suppressWarnings(merge(dtLower, ret$index, by.x=names(dtLower)[wIndicesLower],
                     by.y = ret$colNames, all.x=T))
    dtLower[which(is.na(dtLower$freq.y))]$freq.y <- 0
    setnames(dtLower, "freq.x", "freq")
    setnames(dtLower, "freq.y", "contCount")
    
    ######################
    # Normalization count:
    # 
    # w1 w2     w1  w2 w3
    # b  c  --> a   b  c
    #       --> d   b  c
    #          ---------
    #           *   b  *
    #
    cat("###### Normalization count:\n")
    fixedIndices = wIndicesHigher[-c(1, length(wIndicesHigher))]
    ret <- getMarginalizedCount(dtHigher, fixedIndices, T)
    
    cat("  --> Merging lower order column '", names(dtLower)[1], "' with index column '", 
        ret$colNames, "'\nIndex: \n", sep="");  print(ret$index[1,])
    if (is.null(ret$colNames))
        dtLower$totalMarginalized <- rep(ret$index$freq, nrow(dtLower))
    else {
        dtLower <- suppressWarnings(merge(dtLower, ret$index, by.x=names(dtLower)[1],
                         by.y = ret$colNames, all.x=T))
        dtLower[which(is.na(dtLower$freq.y))]$freq.y <- 1
        setnames(dtLower,"freq.x","freq")
        setnames(dtLower,"freq.y","totalMarginalized")
    }

    #Probability computation
    dtLower$logprob <- log10(pmax(dtLower$contCount-discount,0)/dtLower$totalMarginalized)
    dtLower[which(is.infinite(dtLower$logprob))]$logprob <- (-100)
    return(dtLower)
}

#-----------------------------------------------------
# Train backoff weights
#-----------------------------------------------------
# param dt       : a data table with "word1", ... 
#                  and "freq" columns
#       discount : a fixed discount
# return a data table with a probability column
#
trainBackoffWeight <- function() {
    
    
}

###################
# Main
#
#Read cleaned files
sentences <- NULL
for (src in SOURCES) {
    fileName <- sprintf("%s.%s.txt", "en_US", src)
    srcFile <- sprintf("%s/%s/%s", CLEANDIR, "en_US", fileName)
    if (is.null(sentences)) {
        sentences <- readSample(srcFile)
    }
    else
        sentences <- bind_rows(sentences,readSample(srcFile))
}

#Build ngram frequency table
ngramList <- text2ngram(sentences, ngramOrders = c(1,2,3))

#3-grams discounted probabilities
gram3 <- ngramList[[3]]
gram3 <- trainProb(gram3)

#2-grams discounted countinuation probabilities
gram2 <- ngramList[[2]]
gram2 <- trainContinuationProb(gram2, gram3)

#1-grams discounted countinuation probabilities
gram1 <- ngramList[[1]]
gram1 <- trainContinuationProb(gram1, gram2)

rmobj(fileName)
rmobj(srcFile)
