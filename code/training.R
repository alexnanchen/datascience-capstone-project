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
    df$text <- sapply(df$text, function(s) sprintf("<s> %s </s>",s))
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
    
        #Add id column
        ngrams$id <- seq(1,nrow(ngrams))
        ngrams <- dplyr::select(ngrams, id, token, freq=Freq)
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
getMarginalizedCount <- function(dt, fixedIndices = c(1)) {
    cat("Marginalizing on indices", fixedIndices, "\n")
    print(head(dt, n=2))
    colNames <- names(dt)[fixedIndices]
    index <- dt[,lapply(.SD,sum), by=colNames,.SDcols="freq"]
    setkeyv(index, colNames)
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
    fixedIndices = wIndices[-order]
    ret <- getMarginalizedCount(dt, fixedIndices)
    setkeyv(dt, ret$colNames)
    dt <- dt[ret$index]
    dt$logprob <- log10((dt$freq-discount)/dt$i.freq)
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
trainContinuationProb <- function(dt, discount=0.75) {
    
    
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
trainContinuationProb(gram2)

rmobj(fileName)
rmobj(srcFile)
