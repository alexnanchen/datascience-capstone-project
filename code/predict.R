library(dplyr)
library(data.table)
library(hashFunction)
library(testit)
library(tidyr)

###################
# Environment
#
source("code/lm.R")
source("code/constants.R")

READBUFFER       = 5000
DEBUG            = T
###################
# Implementation
#
#-----------------------------------------------------
# Read a n-gram model from disk
#-----------------------------------------------------
# param fileName  : the n-gram text file
# return a data table
#
readModel <- function(fileName, nbWords) {
    cat("Reading ", fileName,"\n")
    
    #File size in lines
    cmd <- sprintf("wc -l %s", fileName)
    nbLines <- as.integer(strsplit(system(cmd, intern = T),fileName))
    
    #How many buffers do we have to read
    nbBuffers <- (nbLines %/% READBUFFER) + as.integer((nbLines %% READBUFFER) > 0)
    cat(nbLines, "lines to read into", nbBuffers, "buffers of", READBUFFER,"sizes\n")
    
    #Read all buffers
    dt <- NULL; skip <- 0
    for (i in seq(1,nbBuffers)) {
        dt <- readBuffer(fileName, skip, dt, nbWords)
        skip <- skip + READBUFFER
    } 
   
    assert(nrow(dt) == nbLines)
    
    return(dt)
}

#-----------------------------------------------------
# Read a buffer of n-grams
#-----------------------------------------------------
# param fileName  : the n-gram text file
#       skip      : number of rows to skip
#       df        : where to append the rows buffer
# return a data frame
#
readBuffer <- function(fileName, skip, df, nbWords) {
    #Read in memory buffer
    dfBuffer <- tbl_df(read.table(fileName,allowEscapes = T, sep="|", 
                        stringsAsFactors = F, nrow=READBUFFER, skip=skip))
    
    into <- sapply(seq(1,nbWords),function(x) sprintf("w%s", x))
    
    #No context for unigrams
    if (length(into) > 1) {
        dfBuffer <- tidyr::unite_(separate_(dfBuffer,"V1",into=into,sep=" "), col=c("context"), 
                                  from=into[-length(into)], sep=" ")
    
        #Word column
        if ("V3" %in% names(dfBuffer))
            dfBuffer <- dplyr::rename_(dfBuffer, word=into[length(into)], logprob="V2", backoffWeight="V3")
        else
            dfBuffer <- dplyr::rename_(dfBuffer, word=into[length(into)], logprob="V2")
        
        #Compress values to save memory space
        dfBuffer$context <- apply(dfBuffer, 1, function(x) return(ngram2hash(x["context"])))
            
    } else
        dfBuffer <- dplyr::rename_(dfBuffer,word="V1",logprob="V2", backoffWeight="V3")
    
    #Append buffer
    if (is.null(df))
        df <- dfBuffer
    else 
        df <- rbindlist(list(df, dfBuffer))

    return(df)
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

#-----------------------------------------------------
# Retrieve the diff with unigrams
#-----------------------------------------------------
# param dt1     : the unigram table
#       dtRight : the table to diff
# return a table of words missing from dtRight
#
unigramDiff <- function(dt1, dtRight) {
    dtMissing <- suppressWarnings(merge(dt1, dtRight, by.x="word", by.y="word", 
                                        all.x = T)[is.na(logprob.y)])
    return(dtMissing[,.(word)])
}

###################
# Main
#

dt1 <- readModel("gram1.txt",1)
dt2 <- readModel("gram2.txt",2)
dt3 <- readModel("gram3.txt",3)
dt4 <- readModel("gram4.txt",4)
l <- list(dt1, dt2, dt3, dt4)

#print(getBackoffWeight(dt3,"source of many"))

dfResult <- predict("what do you")
print(arrange(dfResult, desc(order), desc(logprob)))

