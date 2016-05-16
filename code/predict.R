library(dplyr)
library(data.table)
library(testit)

###################
# Environment
#
source("code/config.R")
source("code/constants.R")
source("code/mod.R")

READBUFFER = 5000

###################
# Implementation
#
#-----------------------------------------------------
# Read a n-gram model from disk
#-----------------------------------------------------
# param fileName  : the n-gram text file
# return a data table
#
readModel <- function(fileName) {
    cat("Reading ", fileName,"\n")
    
    #File size in lines
    cmd <- sprintf("wc -l %s", fileName)
    nbLines <- as.integer(strsplit(system(cmd, intern = T),fileName))
    
    #How many buffers do we have to read
    nbBuffers <- (nbLines %/% READBUFFER) + as.integer((nbLines %% READBUFFER) > 0)
    
    cat(nbLines, "lines to read into", nbBuffers, "buffers of", READBUFFER,"sizes\n")
    
    #Read all buffers
    df <- NULL; skip <- 0
    for (i in seq(1,nbBuffers)) {
        df <- readBuffer(fileName, skip, df)
        skip <- skip + READBUFFER
    } 
   
    assert(nrow(df) == nbLines)
    
    #Column names renaming
    if ("backoffWeight" %in% names(df))
        df <- dplyr::select(df, ngram=V1, logprob=V2, backoffWeight=V3)
    else
        df <- dplyr::select(df, ngram=V1, logprob=V2)
    
    return(data.table(df))
}

#-----------------------------------------------------
# Read a buffer of n-grams
#-----------------------------------------------------
# param fileName  : the n-gram text file
#       skip      : number of rows to skip
#       df        : where to append the rows buffer
# return a data frame
#
readBuffer <- function(fileName, skip, df) {
    #Read in memory buffer
    dfBuffer <- tbl_df(read.table(fileName,allowEscapes = T, sep="|", 
                        stringsAsFactors = F, nrow=READBUFFER, skip=skip))
    if (is.null(df))
        df <- dfBuffer
    else
        df <- bind_rows(df, dfBuffer)
    return(df)
}

#-----------------------------------------------------
# Compress data a frame
#-----------------------------------------------------
# param df  : a data frame
# return a compressed data frame
#
compressBuffer <- function(df) {
    
}

#-----------------------------------------------------
# Hash a n-gram string
#-----------------------------------------------------
# param df  : a data frame
# return a hash string
#
ngram2hash <- function(ngram) {
    
    
}

#-----------------------------------------------------
# Pack two doubles into one integer
#-----------------------------------------------------
# param logprob       : a negative double
#       backoffWeight : a negative double
# return an integer
#
stats2integer <- function(logprob, backoffWeight) {
    
    
}

#-----------------------------------------------------
# Unpack one integer into two doubles
#-----------------------------------------------------
# param probBackoff : an negative integer containing 
#                     two doubles 
#                     
# return a vector of two doubles logprob, 
#        backoffWeight
#
integer2stats <- function(probBackoff) {
    
    
}


###################
# Main
#
dt <- readModel("gram1.txt")
dt <- readModel("gram2.txt")
dt <- readModel("gram3.txt")
