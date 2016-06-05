library(dplyr)
library(caret)
library(kernlab)
library(testit)

###################
# Environment
#
source("code/config.R")
source("code/mod.R")

#---------------------------------------------------
# Sample sets base of words length frequencies
#---------------------------------------------------
# param df    : a data frame containing sentences
# param dev   : percentage for dev
# param test  : percentage for test
#
# return a list of train, dev and test sentences
#
sampleSets <- function(df, dev=0.2, test=0.1) {
    #Total available sentences
    totalSentences <- nrow(df)
    
    #Sentences by length
    sentByLengthList <- split(df,as.factor(df$nbwords))
    
    #Train, dev and test sets
    dfSets <- list(train=NULL, dev=NULL, test=NULL)
    
    for (dfClass in sentByLengthList) {
        nbRow <- nrow(dfClass)
        if (nbRow < 10) {
            cat("Not enough sentences for class", dfClass$nbwords[0],"skipping\n")
            next
        }
            
        #Sets indices
        inList <- getSetsPartition(nbRow, dev, test)
        
        if (is.null(dfSets$train)) {
            dfSets$train <- dfClass[inList$inTrain,]
            dfSets$dev <- dfClass[inList$inDev,]
            dfSets$test <- dfClass[inList$inTest,]
        }
        else {
            dfSets$train <- rbind(dfSets$train, dfClass[inList$inTrain,])
            dfSets$dev <- rbind(dfSets$dev, dfClass[inList$inDev,])
            dfSets$test <- rbind(dfSets$test, dfClass[inList$inTest,])
        }
    }
    
    assert(totalSentences >= (nrow(dfSets$train) + nrow(dfSets$dev) + 
                                  nrow(dfSets$test)))
    return(dfSets)
}

#---------------------------------------------------
# Get train, dev and test partition indices
#---------------------------------------------------
# param nbSentencesClass : how many sentences to sample
# param dev              : percent for dev
# param test             : percent for test
#
# return a list of indices, one for each set
#
getSetsPartition <- function(nbSentencesClass, dev, test) {
    #Some information
    train <- 1-dev-test
    
    #Vector of all indices
    allIndices <- seq(1, nbSentencesClass)
    
    #Train set
    inTrain <- createDataPartition(allIndices, p=train, list = F)
    inDevTest <- allIndices[-inTrain]
    
    dev <- dev/(dev+test)
    
    #Dev set
    inDev <- createDataPartition(inDevTest, p=dev, list = F)
    inTest <- inDevTest[-inDev]
    
    return(list(inTrain=inTrain,inDev=inDev,inTest=inTest))
}

#---------------------------------------------------
# Output all sets sentences in separate files
#---------------------------------------------------
outputSentences <- function(setsList, dataDir, lang, src) {
    for (setName in c("train", "dev", "test")) {
        destFile <- sprintf("%s/%s/%s_%s_%s.txt", dataDir, lang, lang, src, setName)
        cat("Outputing", destFile, "\n")
        dfSelected <- setsList[[setName]]
        write.table(dfSelected, destFile, sep="|", row.names = F, col.names = F, quote=T, qmethod = "double")
    }
}

###################
# Main
#
for (lang in LANGUAGES) {
    for (src in SOURCES) {
        #Input and output names
        fileName <- sprintf("%s.%s.txt", lang, src)
        srcFile <- sprintf("%s/%s/%s", DATADIR, lang, fileName)
        
        #Read and sample
        df <- readSentences(srcFile, 3, 50)
        setsList <- sampleSets(df, dev=0.2, test=0.1)
        
        #Output selected sentences
        dir.create(sprintf("%s/%s",DATADIR,lang), showWarnings = F)
        outputSentences(setsList, DATADIR, lang, src)
    }
} 

rmobj("df")
rmobj("setsList")

