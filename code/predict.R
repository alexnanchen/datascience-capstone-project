library(dplyr)
library(data.table)
library(testit)

###################
# Environment
#
source("code/mod.R")
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
    df <- NULL; skip <- 0
    for (i in seq(1,nbBuffers)) {
        df <- readBuffer(fileName, skip, df)
        skip <- skip + READBUFFER
    } 
   
    assert(nrow(df) == nbLines)
    
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
    
    #Compress values to save memory space
    dfBuffer <- compressBuffer(dfBuffer)
    
    #Append buffer
    if (is.null(df))
        df <- dfBuffer
    else
        df <- bind_rows(df, dfBuffer)
    return(df)
}

###################
# Main
#
dt1 <- readModel("gram1.txt")
dt2 <- readModel("gram2.txt")
dt3 <- readModel("gram3.txt")

model = list(gram1=dt1,gram2=dt2,gram3=dt3)
