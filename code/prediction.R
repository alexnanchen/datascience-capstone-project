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
# Predict next word from a sentence input
#-----------------------------------------------------
# param strSentence : a sequence of words
# return an ordered data frame
#
predictNextWord <- function(strSentence, dictionary, maxOrder=4) {
    wordsList <- replaceUnknown(strsplit(strSentence, " ")[[1]], dictionary)
    startIndice <- max(length(wordsList)-maxOrder+2, 0)
    ngramContext <- paste(wordsList[startIndice:length(wordsList)], collapse=" ")
    dfResult <- predict(ngramContext) %>% dplyr::filter(!word%in% c("<unk>")) %>%
        mutate(prob=10^logprob)
    dfResult <- as.data.table(dfResult)
    cat("Done predicting\n")
    dfResult <- dfResult[,.(confidence=mean(prob), order=as.integer(max(order))), by=word]
    dfResult <- dfResult[order(-order,-confidence)]
    #dfResult <- group_by(dfResult,word) %>% summarize(confidence=mean(prob), order=as.integer(max(order)))  %>%
    #    arrange(desc(order), desc(confidence))
    cat("Done grouping\n")
    normFactor <- sum(head(dfResult,n=10)$confidence)
    dfResult$confidence[1:10] <- sprintf("%0.2f%%", round(dfResult$confidence[1:10]/normFactor*100,2))
    
    return(head(dfResult,50))
}
