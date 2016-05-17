###################
# Environment
#
source("code/config.R")

MODELPRECISION   = 6
DOUBLEPRECISION  = 1000000     #Six digits
MINLOGPROB       = -99.999999
MAXINT           = DOUBLEPRECISION * trunc(-round(MINLOGPROB))

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
getLog <- function(ngram, model) {
    
}

#-----------------------------------------------------
# Get backoff value associated with 'ngram'
#-----------------------------------------------------
# Ngram is assumed to be existing in model
getBow <- function(ngram, model) {
    
}

#-----------------------------------------------------
# Get log and backoff values associated with 'ngram'
#-----------------------------------------------------
# Ngram is assumed to be existing in model
getLogBow <- function(ngram, model) {
    
}

#-----------------------------------------------------
# Check if 'ngram' is in model
#-----------------------------------------------------
isInModel <- function(ngram, model) {
    
}

#-----------------------------------------------------
# Get 'ngram' log prob, backing off as necessary
#-----------------------------------------------------
getNgramLog <- function(ngram, backoffWeight, model) {
    
    
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
# return the log prob of word at indice 'indice'
getLog <- function(wordsList, indice, model) {
    
    
}
