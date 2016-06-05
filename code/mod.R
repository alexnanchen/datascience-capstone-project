###################
# Environment
#
source("code/config.R")

###################
# General
#
#-----------------------------------------------------
# Utility function to remove an object
#-----------------------------------------------------
rmobj <- function(obj) {
    if (exists(obj)) {
        rm(list=obj, envir =.GlobalEnv)
    }
}

#-----------------------------------------------------
# Utility function to give back memory to OS
#-----------------------------------------------------
runGC <- function() {
    for(i in seq(1,10)) gc()
}

#-----------------------------------------------------
# Interpolate with lower probability
#-----------------------------------------------------
# MITLM interpolate probabilities.
# Can be used to check that not interpolated values
# match interpolated values.
# see https://github.com/mitlm/mitlm/blob/master/src/KneserNeySmoothing.cpp
interpolate <- function(highestProb, lowerProb, lowerBow) {
    pLower <- 10^(lowerBow+lowerProb)
    pHigher <- 10^highestProb
    return(log10(pHigher+pLower))
}

###################
# Data
#
#---------------------------------------------------
# Read sentences from a text file 
#---------------------------------------------------
# param fileName  : the source text file
#       minLength : min words per sentence
#       maxLength : max words per sentence
#
# return a data frame of sentences and words count
#
readSentences <- function(fileName, minLength, maxLength) {
    #Read whole file
    cmd <- sprintf("awk '{if (NF>=%d && NF <=%d) print $0}' < %s", minLength, maxLength, fileName)
    cat("Reading file with following command ", cmd)
    con <- pipe(cmd,encoding = "UTF-8")
    df <- tbl_df(data.frame(text =readLines(con), stringsAsFactors = F))
    close(con)
    
    #Nb words stat
    df$nbwords <- apply(df,1,function(r){length(strsplit(r," +")[[1]])})
    return(df)
}

###################
# Model
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
# Save a compress model to disk
#-----------------------------------------------------
# param dt       : a data table
#       fileName : where to save the model
saveModel <- function(dt, fileName) {
    cat("Save model to", fileName, "\n")
    write.table(dt, fileName, col.names = T, row.names = F, sep="|", quote=T,
                qmethod="double", fileEncoding = 'utf-8')
}

#-----------------------------------------------------
# Read a compress model from disk
#-----------------------------------------------------
# param fileName : compress model
readCompressed <- function(fileName) {
    cat("Reading model", fileName, "\n")
    dtCompressed <- read.table(fileName,allowEscapes = T, sep="|", 
                               stringsAsFactors = F)
    return(dtCompressed)
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

###################
# Prediction
#
#-----------------------------------------------------
# Replace unknown words by <unk>
#-----------------------------------------------------
replaceUnknown <- function(wordsList, dictionary) {
    for (i in seq(1, length(wordsList))) {
        #print(head(dictionary))
        w <- tolower(wordsList[i])
        if (nrow(dictionary[word1==w])==0)
            wordsList[i] <- stri_enc_toutf8("<unk>")
        else
            wordsList[i] <- w
    }
    return(wordsList)
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
