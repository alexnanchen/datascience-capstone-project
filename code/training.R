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
readCounts <- function(fileName) {
    cat("Reading ", fileName,"\n")
    df <- tbl_df(read.table(fileName, header=T, allowEscapes = T, sep="\t", stringsAsFactors = F, 
                 fileEncoding = "UTF-8", encoding = "UTF-8"))
    return(as.data.table(df))
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
trainProb <- function(dt, mincount=0) {
    wIndices <- grep("word*",names(dt))
    order <- length(wIndices)
    
    cat("###### Normal counts, order", order, "\n")
    #Discount on effective counts and on unpruned ngrams
    discount <- getDiscount(dt, col = "freq")
    cat("  --> Using discount value of", discount, "\n")
    #Model pruning before marginalization count
    cat("  --> Prune with min count value of", mincount,"\n")
    dt <- dt[freq>mincount]
    
    #Model estimation
    cat("  --> Get marginalized counts\n")
    fixedIndices = wIndices[-order]
    ret <- getMarginalizedCount(dt, fixedIndices, F)
    setkeyv(dt, ret$colNames)
    dt <-  suppressWarnings(merge(dt, ret$index, all.x=T))
    #print(head(dt))
    cat("  --> Compute probabilities\n")
    setnames(dt, "freq.x", "freq")
    setnames(dt, "freq.y", "totalMarginalized")
    #Probability computation
    dt$logprob <- log10(pmax(dt$freq-discount,0)/dt$totalMarginalized)
    return(list(dt=dt,discount=discount))
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
trainContinuationProb <- function(dtLower, dtHigher, mincount=0) {
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
    
    #All grams have been observed at least once
    assert(all(dtLower$contCount>0))
   
    #Unigram, set "<s>" contCount to 0 before normalization count
    if (length(wIndicesLower) == 1)
        dtLower["<s>"]$contCount <- 0
    
    #Discount on effective counts
    discount <- getDiscount(dtLower, col = "contCount")
    cat("  --> Using discount of", discount, "\n")
    
    #Model pruning before marginalization count
    cat("  --> Prune with min count value of", mincount,"\n")
    dtLower <- dtLower[contCount>mincount]
    
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
    
    #Some normalization
    infIndices <- which(is.infinite(dtLower$logprob))
    dtLower[infIndices]$logprob <- -99
    
    #Start symbol cannot have a probability
    if (is.null(ret$colNames))
        dtLower["<s>"]$logprob <- -99
    
    return(list(dt=dtLower,discount=discount))
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
getDiscount <- function(dtCounts, col="freq") {
    n1 <- length(which(dtCounts[,col, with=F] == 1))
    n2 <- length(which(dtCounts[,col, with=F] == 2))
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
#Read counts files
gram1 <- readCounts("counts1.txt")
gram2 <- readCounts("counts2.txt")
gram3 <- readCounts("counts3.txt")
gram4 <- readCounts("counts4.txt")

cat("Start training\n")
cat("4 grams training\n")
#4-grams discounted probabilities
ret <- trainProb(gram4, mincount = 1)
gram4 <- ret$dt; D4 <- ret$discount

#3-grams discounted probabilities
cat("3 grams training\n")
#gram3 <- trainProb(gram3, mincount = 1)
ret <- trainContinuationProb(gram3, gram4, mincount=1)
gram3 <- ret$dt; D3 <- ret$discount

#2-grams discounted countinuation probabilities
cat("2 grams training\n")
ret <- trainContinuationProb(gram2, gram3, mincount=2)
gram2 <- ret$dt; D2 <- ret$discount

#1-grams discounted countinuation probabilities
#Fixed vocabulary, no discount (see log10(1/42570)) or pruning
cat("1 grams training\n")
ret <- trainContinuationProb(gram1, gram2, mincount=0)
gram1 <- ret$dt; D1 <- ret$discount

cat("Backoff weight computation\n")
#3-grams backoff weights, discount is D4
gram3 <- trainBackoffWeight(gram3, gram4, D4)

#2-grams backoff weights
gram2 <- trainBackoffWeight(gram2, gram3, D3)

#1-grams backoff weights
gram1 <- trainBackoffWeight(gram1, gram2, D2)

cat("1-grams", nrow(gram1), "\n2-grams", nrow(gram2), "\n3-grams", nrow(gram3),
    "\n4-grams", nrow(gram4), "\n")

cat("Discount of ", D4, D3, D2, "\n")

#Models serialization
cat("Writing models to disk\n")
writeModel(gram1)
writeModel(gram2)
writeModel(gram3)
writeModel(gram4)

rmobj("fileName")
rmobj("srcFile")
