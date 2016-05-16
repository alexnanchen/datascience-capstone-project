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
# Utility function to remove an object
#-----------------------------------------------------
rmobj <- function(obj) {
    if (exists(obj))
        rm(obj)
}

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

#-----------------------------------------------------
# Some basic checks
#-----------------------------------------------------
# Could be implemented as a unit test.
#
testAll <- function() {
    i1 <- -9.9999967; i2 <- -98.255557
    print("Packing ") 
    cat(" -->"); print(i1, digits=8)
    cat(" -->"); print(i2, digits=8)
    i <- stats2double(i1,i2); cat(" -->"); print(i, digits=20)
    print(object.size(i))
    cat("Unpacking", i, " ")
    print(integer2stats(i), digits = 8)
    cat("Hashing 'Hello économie'", ngram2hash("Hello économie"))
}

###################
# Main
#
#testAll()
