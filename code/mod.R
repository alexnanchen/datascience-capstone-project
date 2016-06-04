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
