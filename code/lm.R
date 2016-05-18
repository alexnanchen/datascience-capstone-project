###################
# Environment
#
source("code/config.R")

MODELPRECISION   = 6
DOUBLEPRECISION  = 1000000     #Six digits
MINLOGPROB       = -99.999999
MAXINT           = DOUBLEPRECISION * trunc(-round(MINLOGPROB))
DEBUG            = T

###################
# Implementation
#
#-----------------------------------------------------
# Compress data ngram model
#-----------------------------------------------------
# param df  : a ngram data frame
# return a compressed data frame
#
compressBuffer <- function(df) {
    #Empty backoff column
    if (!"V3" %in% names(df))
        df$V3 <- rep(0, nrow(df))
    
    #Rename for readability
    df <- dplyr::select(df, ngram=V1, stats=V2, backoffWeight=V3)
    
    #Data compression
    df$ngram <- apply(df, 1, function(x) return(ngram2hash(x["ngram"])))
    df$stats <- apply(df, 1, function(x) return(stats2double(x["stats"], x["backoffWeight"])))
    
    return(select(df, ngram, stats))
}

#-----------------------------------------------------
# Hash a n-gram string
#-----------------------------------------------------
# param ngram  : a character vector
# return an integer value
#
ngram2hash <- function(ngram) {
    return(spooky.32(ngram))
}

#-----------------------------------------------------
# Pack two doubles into one double
#-----------------------------------------------------
# param logprob       : a negative double
#       backoffWeight : a negative double
# return a combined double
#
stats2double <- function(logprob, backoffWeight) {
    #Some checks
    assert(logprob < 0)
    assert(backoffWeight <= 0)
    
    #Bound log probs
    if (logprob < MINLOGPROB)
        logprob = MINLOGPROB
    if (backoffWeight < MINLOGPROB)
        backoffWeight = MINLOGPROB
    
    #Round and convert to unsigned int
    intProb = -double2int(logprob)
    intBackoffWeight = -double2int(backoffWeight)
    
    #Integer combination into one "int double" of 48 bytes
    doubleValue = trunc(as.double(intProb + MAXINT * intBackoffWeight))
    
    return(doubleValue)
}

#-----------------------------------------------------
# Unpack one double into two doubles
#-----------------------------------------------------
# param combinedDouble : a negative double
# return c(logprob, backoffweight)
#
double2stats <- function(combinedDouble) {
    #Some check
    assert(combinedDouble>0)
    
    #Double conversion. May have some small double error
    logProb = -(combinedDouble %% MAXINT) / DOUBLEPRECISION
    backoffWeight = -(combinedDouble %/% MAXINT) / DOUBLEPRECISION
    
    return (c(logProb, backoffWeight))
}

#-----------------------------------------------------
# Truncate to 6 decimal places and convert to int
#-----------------------------------------------------
# Make sure that there are at max. 6 decimal values
# param value : a double
double2int <- function(value) {
    return(as.integer(trunc(value*DOUBLEPRECISION)))
}

#-----------------------------------------------------
# Get log value associated with 'ngram'
#-----------------------------------------------------
# Ngram is assumed to be existing in model
getLogValue <- function(ngram, model) {
    return(getLogBow(ngram, model)[1])
}

#-----------------------------------------------------
# Get backoff value associated with 'ngram'
#-----------------------------------------------------
# Ngram is assumed to be existing in model
getBowValue <- function(ngram, model) {
    return(getLogBow(ngram, model)[2])
}

#-----------------------------------------------------
# Get log and backoff values associated with 'ngram'
#-----------------------------------------------------
# Ngram is assumed to be existing in model
getLogBow <- function(ngram, model) {
    h <- ngram2hash(ngram)
    #print(sprintf(">%s< --> %d\n", ngram, h))
    return(double2stats(model[ngram==h]$stats))
}

#-----------------------------------------------------
# Check if 'ngram' is in model
#-----------------------------------------------------
isInModel <- function(ngram, model) {
    h <- ngram2hash(ngram)
    return(nrow(model[ngram==h])>0)
}

#-----------------------------------------------------
# Extract an n-gram ending at 'indice'
#-----------------------------------------------------
getNgramFromList <- function(wordsList, indice, maxorder) {
    startIndice <- indice-maxorder+1
    startIndice <- if(startIndice>0) startIndice else 1
    return (paste(wordsList[startIndice:indice], collapse=" "))   
}

#-----------------------------------------------------
# Get 'ngram' log prob, backing off as necessary
#-----------------------------------------------------
getNgramLog_ <- function(ngram, backoffWeight, model, prefix="   ") {
    #Some preparation
    wordsList <- strsplit(ngram," ")[[1]]
    order <- length(wordsList)
    
    #Stop conditions: found or unknown
    if (isInModel(ngram, model)) {
        logProb <- getLogValue(ngram,model)
        if(DEBUG) cat(prefix, "Found ngram '",ngram,"', ", logProb, "\n")
        return(logProb+backoffWeight)
    }
    else if (order<=1) {
        if(DEBUG) cat(prefix, "Unknown log probability!\n")
        return(NULL)
    }
    
    #Backoff
    order <- order - 1
    ngram <- paste(wordsList[-1], collapse=" ")
    backoffContext <- paste(wordsList[-(length(wordsList))], collapse=" ")
    
    bow <- 0
    if (isInModel(backoffContext, model)) {
        bow <- getBowValue(backoffContext, model)
        if(DEBUG) cat(prefix, "Found backoff '", backoffContext,"', ", bow, "\n")
    }
    
    if(DEBUG) cat(prefix, "Backing off to level", order, "\n")
    return(getNgramLog_(ngram, backoffWeight + bow, model, paste(prefix," ")))
}

###################
# Public interface
#
#-----------------------------------------------------
# Get log probability of word at indice 'indice'
#-----------------------------------------------------
# param wordsList : a character vector
#       indice    : an indice into the vector
#       model     : a data table model
#       maxorder  : the highest model order
# return the log prob of word at indice 'indice'
getNgramLog <- function(wordsList, indice, model, maxorder) {
    ngram <- getNgramFromList(wordsList, indice, maxorder)
    if(DEBUG) cat("    Get log probability for '", ngram, "'\n")
    return(getNgramLog_(ngram,0.0, model))
}

#-----------------------------------------------------
# Get joint probability of a sequence of words
#-----------------------------------------------------
# param strSentence : a character vector
#       model       : a data table model
#       maxorder    : the highest model order
# return the joint log probability with oov count
getSentenceLog <- function(strSentence, model, maxorder) {
    strSentence <- paste("<s>",strSentence,"</s>")
    if(DEBUG) cat("==> Scoring", strSentence, "\n")
    
    wordsList <- strsplit(strSentence," ")[[1]]
    totalLog <- 0; oov<- 0
    
    #Do not select start symbol
    for (i in seq(2, length(wordsList))) {
        logProb <- getNgramLog(wordsList,i, model, maxorder)
        if(is.null(logProb))
            oov = oov + 1
        else
            totalLog = totalLog + logProb
    }
    return(list(totalLog=totalLog, oov=oov))
}
