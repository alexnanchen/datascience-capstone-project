library(dplyr)
library(data.table)
library(RWeka)
library(tidyr)

###################
# Environment
#
source("code/mod.R")
source("code/constants.R")
source("code/lm.R")

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
getMarginalizedCount <- function(dt, fixedIndices = c(1), cCount = F, col="freq") {
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
        index <- dt[,lapply(.SD,sum),.SDcols=col]
    else {
        colNames <- names(dt)[fixedIndices]
        index <- dt[,lapply(.SD,sum), by=colNames,.SDcols=col]
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
trainProb <- function(dt, discount) {
    wIndices <- grep("word*",names(dt))
    order <- length(wIndices)
    cat("###### Normal counts, order", order, "\n")
    fixedIndices = wIndices[-order]
    ret <- getMarginalizedCount(dt, fixedIndices, F)
    setkeyv(dt, ret$colNames)
    dt <-  suppressWarnings(merge(dt, ret$index, all.x=T))
    #print(head(dt))
    setnames(dt, "freq.x", "freq")
    setnames(dt, "freq.y", "totalMarginalized")
    dt$logprob <- log10(pmax(dt$freq-discount,0)/dt$totalMarginalized)
    return(dt)
}

#-----------------------------------------------------
# Train continuation probabilities
#-----------------------------------------------------
# param dtLower  : a data table with "word1", ... 
#                  and "freq" columns
# param dtHigher : a data table with "word1", ... 
#                  and "freq" columns
#       discount : a fixed discount
# return a data table with a probability column
#
trainContinuationProb <- function(dtLower, dtHigher, discount) {
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
    setnames(dtLower, "freq.x", "freq")
    setnames(dtLower, "freq.y", "contCount")
    
    #Updade missing value with normal counts
    #No missing value should be left, otherwise NA + integer = NA
    nasIndices <- which(is.na(dtLower$contCount))
    #See mitlm https://github.com/mitlm/mitlm/blob/master/src/KneserNeySmoothing.cpp
    #Initialize()
    dtLower[nasIndices]$contCount <- dtLower[nasIndices]$freq
   
    #Unigram, set "<s>" contCount to 0 before normalization count
    if (length(wIndicesLower) == 1)
        dtLower["<s>"]$contCount <- 0
    
    ######################
    # Normalization count:
    # 
    # w1 w2     w1  w2 w3
    # b  c  --> a   b  c
    #       --> d   b  c
    #          ---------
    #           *   b  *
    # Instead of marginalizing on higher order n-gram, we can 
    # marginalize on lower order because w1 has already been 
    # marginalized. That way the conditional probability will
    # sum to one.
    # 
    # w1 w2
    # b  c   --> b *  (marginalization of last word)
    #
    cat("###### Normalization count:\n")
    #Remove one to adapt to lower gram. Empty vector-1 = empty vector
    fixedIndices = wIndicesLower[-length(wIndicesLower)]
    ret <- getMarginalizedCount(dtLower, fixedIndices, F, "contCount")
    cat("  --> Merging lower order column '", names(dtLower)[fixedIndices], "' with index column '", 
        ret$colNames, "'\nIndex: \n", sep="");  print(ret$index[1,])
    
    #Unigram case
    if (is.null(ret$colNames))
        dtLower$totalMarginalized <- rep(ret$index$contCount, nrow(dtLower))
    else {
        dtLower <- suppressWarnings(merge(dtLower, ret$index, by.x=names(dtLower)[fixedIndices],
                         by.y = ret$colNames, all.x=T))
        setnames(dtLower,"contCount.x","contCount")
        setnames(dtLower,"contCount.y","totalMarginalized")
    }
    
    #Probability computation
    dtLower$logprob <- log10(pmax(dtLower$contCount-discount,0)/dtLower$totalMarginalized)
    
    #Start symbol cannot have a probability
    if (is.null(ret$colNames))
        dtLower["<s>"]$logprob <- -99
    
    return(dtLower)
}

#-----------------------------------------------------
# Train backoff weights
#-----------------------------------------------------
# param dtLower  : a data table with "word1", ... 
#                  and "freq" columns
# param dtHigher : a data table with "word1", ... 
#                  and "freq" columns
#       discount : a fixed discount
# return a data table with a probability column
#
trainBackoffWeight <- function(dtLower, dtHigher, discount) {
    wIndicesLower <- grep("word*",names(dtLower))
    wIndicesHigher <- grep("word*",names(dtHigher))
    
    cat("###### Budget contributors:\n")
    fixedIndices = wIndicesHigher[-length(wIndicesHigher)]
    ret <- getMarginalizedCount(dtHigher, fixedIndices, T)
    
    cat("  --> Merging lower order column '", names(dtLower)[wIndicesLower], "' with index column '", 
        ret$colNames, "'\nIndex: \n", sep=""); print(ret$index[1,])
    
    setkeyv(dtLower, names(dtLower)[wIndicesLower])
    dtLower <- suppressWarnings(merge(dtLower, ret$index, by.x=names(dtLower)[wIndicesLower],
                                      by.y = ret$colNames, all.x=T))
    dtLower[which(is.na(dtLower$freq.y))]$freq.y <- 0
    setnames(dtLower, "freq.x", "freq")
    setnames(dtLower, "freq.y", "budgetContributors")
    
    cat("###### Discount probabilities:\n")
    cat("  --> Merging lower order column '", names(dtLower)[wIndicesLower], "' with higher order column '", 
        names(dtHigher)[fixedIndices], "'\n", sep="");
    
    #All values are the same, use max function
    colNames <- names(dtHigher)[fixedIndices]
    dtTmp <- dtHigher[,lapply(.SD,max), by=colNames,.SDcols="totalMarginalized"]
    
    dtLower <- suppressWarnings(merge(dtLower, dtTmp, by.x=names(dtLower)[wIndicesLower],
                                      by.y = colNames , all.x=T))
    
    setnames(dtLower, "totalMarginalized.x", "totalMarginalized")
    setnames(dtLower, "totalMarginalized.y", "discountNorm")
    dtLower[which(is.na(dtLower$discountNorm))]$discountNorm <- 1
    
    dtLower$backoffWeight <- log10(discount/dtLower$discountNorm*dtLower$budgetContributors)
    dtLower[which(is.infinite(dtLower$backoffWeight))]$backoffWeight <- 0
    
    return(dtLower)
}

#-----------------------------------------------------
# Get fixed discount
#-----------------------------------------------------
# Formula: n1/(n1+2*n2)
# param dtHigher : the table counts to be discounted
# return the discount estimate
getDiscount <- function(dtHigher, col="freq") {
    n1 <- length(which(dtHigher[,col, with=F] == 1))
    n2 <- length(which(dtHigher[,col, with=F] == 2))
    if (n1 == 0 && n2 == 0)
        return(1.0)
    return(n1/(n1+2*n2))
}

#-----------------------------------------------------
# Write a n-gram model to disk
#-----------------------------------------------------
# param dt           : a data table
#       highestOrder : is it the top level model
writeModel <- function(dt) {
    #Word merge
    wIndices <- grep("word*",names(dt))
    dt <- unite_(dt,"ngram", names(dt)[wIndices], sep=" ")
    
    #Columns selection
    tblDf <- NULL
    if (!"backoffWeight" %in% names(dt))
        tblDf <- select(tbl_df(dt), ngram, logprob)
    else {
        tblDf <- select(tbl_df(dt), ngram, logprob, backoffWeight)
        tblDf$backoffWeight  <- round(tblDf$backoffWeight,MODELPRECISION)
    }
    
    #4-decimal places rounding
    tblDf$logprob  <- round(tblDf$logprob,MODELPRECISION)
    
    #Data serialization
    strName <- sprintf("gram%d.txt", length(wIndices))
    write.table(tblDf, strName, col.names = F, row.names = F, sep="|", quote=T,
                qmethod="double", fileEncoding = 'utf-8')
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

#Save sentences
write.table(sentences, "training.txt", quote=F, sep="\t", row.names=F,
            col.names=F, fileEncoding = "UTF-8")

#One chunk of text
#sentences <- paste(sentences$text, collapse=" ")

#Build ngram frequency table
cat("N-gram extraction\n")
ngramList <- text2ngram(sentences, ngramOrders = c(1,2,3,4))

#Discounted value
gram4 <- ngramList[[4]]
gram3 <- ngramList[[3]]
gram2 <- ngramList[[2]]
gram1 <- ngramList[[1]]

#Discounts
D4 = getDiscount(gram4)
D3 = getDiscount(gram3)
D2 = getDiscount(gram2)

cat("Start training\n")
#4-grams discounted probabilities, discout is D3
gram4 <- trainProb(gram4, D4)

#3-grams discounted probabilities
#gram3 <- trainProb(gram3, D3)
gram3 <- trainContinuationProb(gram3, gram4, D3)

#2-grams discounted countinuation probabilities
gram2 <- trainContinuationProb(gram2, gram3, D2)

#1-grams discounted countinuation probabilities
#Fixed vocabulary, no discount (see log10(1/42570))
gram1 <- trainContinuationProb(gram1, gram2, 0)

#3-grams backoff weights, discount is D4
gram3 <- trainBackoffWeight(gram3, gram4, D4)

#2-grams backoff weights
gram2 <- trainBackoffWeight(gram2, gram3, D3)

#1-grams backoff weights
gram1 <- trainBackoffWeight(gram1, gram2, D2)

cat("1-grams", nrow(gram1), "\n2-grams", nrow(gram2), "\n3-grams", nrow(gram3),
    "\n4-grams", nrow(gram4), "\n")

cat("Discount of ", D3, D2, "\n")

#Models serialization
cat("Writing models to disk\n")
writeModel(gram1)
writeModel(gram2)
writeModel(gram3)
writeModel(gram4)

rmobj(fileName)
rmobj(srcFile)
