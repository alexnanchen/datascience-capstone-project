library(dplyr)
library(data.table)
library(hashFunction)
library(testit)
library(tidyr)

###################
# Environment
#
source("code/lm.R")

DEBUG = T

###################
# Implementation
#
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
            if(DEBUG) cat(paste0("Search for next word with context >",ngramContext,"<\n"))
            dfResult <- bind_rows(dfResult, dplyr::select(l[[order]], word, logprob) %>% 
                            mutate(logprob=logprob+backoffWeight, order=order))
            if(DEBUG) cat("  =====> Added backoff weight of", backoffWeight, "to", 
                          nrow(l[[order]]), "results\n")
        }
        return(dfResult)
    }
    
    #A backoff weight is necessary to compute
    #probabilities
    if(!is.na(backoffWeight)) {
        if(DEBUG) cat(paste0("Search for next word with context >",ngramContext,"<\n"))
        setkey(l[[order]], context)
        dftmp <- tbl_df(l[[order]][.(ngram2hash(ngramContext))])
        #Update results
        if (!is.na(dftmp[1,"word"])) {
            dfResult <- bind_rows(dfResult, dplyr::select(dftmp, word, logprob) %>% 
                            mutate(logprob=logprob+backoffWeight, order=order))
            if(DEBUG) cat("  =====> Added backoff weight of", backoffWeight, "to", 
                          nrow(dftmp), "results\n")
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
        if(DEBUG) cat("Backoff search key:", wordsList)
        #Index values
        hashContext <- ngram2hash(paste(wordsList[-length(wordsList)],collapse = " "))
        strWord <- wordsList[length(wordsList)]
        if(DEBUG) cat(" -->", hashContext, strWord)
        #Backoff weight retrieval
        setkey(dtLower, context, word)
        filteredDt <- dtLower[.(hashContext, strWord)]
    } else {
        if(DEBUG) cat("Search key:", backoffContext)
        setkey(dtLower, word)
        filteredDt <- dtLower[.(backoffContext)]
    }
    cat(" --> found backoff weight of",filteredDt$backoffWeight, "\n")
    
    #Some weight or NA
    return(filteredDt$backoffWeight)
}
