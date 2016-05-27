library(dplyr)
library(data.table)
library(hashFunction)
library(testit)
library(tidyr)

###################
# Environment
#
source("code/lm.R")

READBUFFER       = 5000
DEBUG            = T
log              = NULL

###################
# Implementation
#
#-----------------------------------------------------
# Read a compress model from disk
#-----------------------------------------------------
# param fileName : compress model
readCompressed <- function(fileName) {
    cat("Reading model", fileName, "\n")
    dtCompressed <- fread(fileName, sep="|", header=T, stringsAsFactors = F, encoding = "UTF-8")
    return(dtCompressed)
}

#-----------------------------------------------------
# Predict using backing off
#-----------------------------------------------------
predict <- function(ngramContext, backoffWeight=0, dfResult=data.frame()) {
    #In with table should we search
    order=1
    if(length(ngramContext)!=0)
        order <- length(strsplit(ngramContext, split = " ")[[1]]) + 1
    
    #Stop condition
    if (order==1) {
        if(!is.na(backoffWeight)) {
            debug(paste0("Search for next word with context >",ngramContext," *<\n"))
            dfResult <- bind_rows(dfResult, dplyr::select(l[[order]], word, logprob) %>% 
                            mutate(logprob=logprob+backoffWeight, order=order))
            debug(sprintf("  =====> Added backoff weight of %f to %d results\n", backoffWeight, 
                          nrow(l[[order]])))
        }
        return(dfResult)
    }
    
    #A backoff weight is necessary to compute
    #probabilities
    if(!is.na(backoffWeight)) {
        debug(paste0("Search for next word with context >",ngramContext," *<\n"))
        setkey(l[[order]], context)
        dftmp <- tbl_df(l[[order]][.(ngram2hash(ngramContext))])
        #Update results
        if (!is.na(dftmp[1,"word"])) {
            dfResult <- bind_rows(dfResult, dplyr::select(dftmp, word, logprob) %>% 
                            mutate(logprob=logprob+backoffWeight, order=order))
            debug(sprintf("  =====> Added backoff weight of %f to %d results\n", backoffWeight,
                          nrow(dftmp)))
        }
    }

    #Get backoff weight in lower order model
    bow <- getBackoffWeight(l[[order-1]], ngramContext)
    
    #Now backoff
    wordsList <- strsplit(ngramContext, split = " ")[[1]]
    newNgramContext <- paste(wordsList[-1], collapse = " ")
    
    #Recursive call
    predict(newNgramContext, bow, dfResult)
}

#-----------------------------------------------------
# Retrieve the backoff weight of a context
#-----------------------------------------------------
# param dtLower        : a data table containin bow
#       backoffContext : a word string
# return a weight or NULL
#
getBackoffWeight <- function(dtLower, backoffContext) {
    backoffWeight <- NULL
    #Greater than unigram
    if ("context" %in% names(dtLower)) {
        wordsList <- strsplit(backoffContext, split = " ")[[1]]
        debug(sprintf("Backoff search key: '%s'", paste(wordsList, collapse=" ")))
        #Index values
        hashContext <- ngram2hash(paste(wordsList[-length(wordsList)],collapse = " "))
        strWord <- wordsList[length(wordsList)]
        debug(sprintf(" -->'%s' '%s'\n", hashContext, strWord))
        #Backoff weight retrieval
        setkey(dtLower, context, word)
        filteredDt <- dtLower[.(hashContext, strWord)]
    } else {
        debug(sprintf("Search key: '%s'\n", backoffContext))
        setkey(dtLower, word)
        filteredDt <- dtLower[.(backoffContext)]
    }
    debug(sprintf(" --> found backoff weight of %f\n",filteredDt$backoffWeight))
    
    #Some weight or NA
    return(filteredDt$backoffWeight)
}

#-----------------------------------------------------
# Display debug message and store a log
#-----------------------------------------------------
debug <- function(strMessage) {
    if(DEBUG) 
        cat(strMessage)
    log <<- c(log, strMessage)
}

#-----------------------------------------------------
# Predict next word from a sentence input
#-----------------------------------------------------
# param strSentence : a sequence of words
# return an ordered data frame
#
predictNextWord <- function(strSentence, maxOrder=4) {
    log <<- c()
    dfResult <- predict(strSentence) %>% dplyr::filter(!word%in% c("<unk>"))
    dfResult <- group_by(dfResult,word) %>% summarize(mean=mean(logprob), 
                        maxorder=max(order)) %>% arrange(desc(maxorder), desc(mean))
    return(list(dfResult=head(dfResult,50), log=log))
}

#Load once all models
MODELDIR <- "/Users/alexnanchen/data-science"
dt1 <- readCompressed(paste0(MODELDIR,"/gram1c.txt"))
dt2 <- readCompressed(paste0(MODELDIR,"/gram2c.txt"))
dt3 <- readCompressed(paste0(MODELDIR,"/gram3c.txt"))
dt4 <- readCompressed(paste0(MODELDIR,"/gram4c.txt"))

l <- list(dt1, dt2, dt3, dt4)

