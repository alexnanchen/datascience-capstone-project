library(dplyr)
library(data.table)
library(hashFunction)
library(testit)

###################
# Environment
#
source("code/lm.R")
source("code/constants.R")

READBUFFER       = 5000

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
    dt <- NULL; skip <- 0
    for (i in seq(1,nbBuffers)) {
        dt <- readBuffer(fileName, skip, dt)
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
readBuffer <- function(fileName, skip, df) {
    #Read in memory buffer
    dfBuffer <- tbl_df(read.table(fileName,allowEscapes = T, sep="|", 
                        stringsAsFactors = F, nrow=READBUFFER, skip=skip))
    
    #Compress values to save memory space
    dfBuffer <- data.table(compressBuffer(dfBuffer))
    
    #Append buffer
    if (is.null(df))
        df <- dfBuffer
    else 
        df <- rbindlist(list(df, dfBuffer))

    return(df)
}

###################
# Main
#
dt1 <- readModel("gram1.txt")
dt2 <- readModel("gram2.txt")
dt3 <- readModel("gram3.txt")

#One table
model = rbindlist(list(dt1,dt2,dt3))

#Ordered index
setkey(model,ngram)
