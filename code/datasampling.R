library(dplyr)
library(tm)

###################
# Environment
#
source("code/config.R")

###################
# Implementation
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

#---------------------------------------------------
# Sample sentences base of words length frequencies
#---------------------------------------------------
# param sampleSize: how many sentences to sample
#
# return a data frame of sentences and words count
#
sampleSentences <- function(df, sampleSize) {
    #Total available sentences
    totalSentences <- nrow(df)
    
    #Sentences by length
    sentByLengthList <- split(df,as.factor(df$nbwords))
    
    dfSelected <- NULL
    for (dfclass in sentByLengthList) {
        nbSentencesClass <- floor(sampleSize * (nrow(dfclass)/totalSentences))
        cat("Selecting ", nbSentencesClass, "out of", sampleSize, "\n")
        
        if (is.null(dfSelected))
            dfSelected <- sample_n(dfclass,nbSentencesClass)
        else
            dfSelected <- rbind(dfSelected, sample_n(dfclass,nbSentencesClass))
    }
    return(dfSelected)
}

###################
# Main
#
MAXSENTPERSRC = 1000
dir.create(SAMPLEDIR, showWarnings = F)

for (lang in LANGUAGES) {
    for (src in SOURCES) {
        #Input and output names
        fileName <- sprintf("%s.%s.txt", lang, src)
        srcFile <- sprintf("%s/%s/%s", DATADIR, lang, fileName)
        destFile <- sprintf("%s/%s/%s", SAMPLEDIR, lang, fileName)
        
        #Read and sample
        df <- readSentences(srcFile, 3, 50)
        dfSelected <- sampleSentences(df, MAXSENTPERSRC)
        
        #Output selected sentences
        dir.create(sprintf("%s/%s",SAMPLEDIR,lang), showWarnings = F)
        cat("Outputing", destFile, "\n")
        write.table(dfSelected, destFile, sep="\t", row.names = F, col.names = F, quote=T)
    }
} 

rm(fileName, srcFile, destFile, df, dfSelected)
