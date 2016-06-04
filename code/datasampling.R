library(dplyr)
library(tm)

###################
# Environment
#
source("code/config.R")
source("code/mod.R")

###################
# Implementation
#
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
        #nbSentencesClass <- nrow(dfclass)
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
MAXSENTPERSRC = 100000

for (lang in LANGUAGES) {
    for (src in SOURCES) {
        #Input and output names
        fileName <- sprintf("%s_%s_train.txt", lang, src)
        srcFile <- sprintf("%s/%s/%s", DATADIR, lang, fileName)
        destFile <- sprintf("%s/%s/%s", SAMPLEDIR, lang, fileName)
        
        #Read and sample
        df <- readSentences(srcFile, 3, 50)
        dfSelected <- sampleSentences(df, MAXSENTPERSRC)
        
        #Output selected sentences
        dir.create(sprintf("%s/%s",SAMPLEDIR,lang), showWarnings = F)
        cat("Outputing", destFile, "\n")
        write.table(dfSelected, destFile, sep="|", row.names = F, col.names = F, quote=T, qmethod = "double")
    }
} 

rm(fileName, srcFile, destFile, df, dfSelected)
