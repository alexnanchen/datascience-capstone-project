library(dplyr)
library(data.table)
library(hashFunction)
library(testit)
library(tidyr)

###################
# Environment
#
source("code/mod.R")
source("code/prediction.R")

###################
# Implementation
#
#-----------------------------------------------------
# Predict next word from a sentence input
#-----------------------------------------------------
# param strSentence : a sequence of words
# return an ordered data frame
#
predictNextWord <- function(strSentence, maxOrder=4) {
    wordsList <- replaceUnknown(strsplit(strSentence, " ")[[1]])
    startIndice <- max(length(wordsList)-maxOrder+2, 0)
    ngramContext <- paste(wordsList[startIndice:length(wordsList)], collapse=" ")
    dfResult <- predict(ngramContext) %>% dplyr::filter(!word%in% c("<unk>")) %>%
        mutate(prob=10^logprob)
    dfResult <- group_by(dfResult,word) %>% summarize(confidence=mean(prob), order=as.integer(max(order)))  %>%
        arrange(desc(order), desc(confidence))
    normFactor <- sum(head(dfResult,n=10)$confidence)
    dfResult$confidence[1:10] <- sprintf("%0.2f%%", round(dfResult$confidence[1:10]/normFactor*100,2))
    
    return(head(dfResult,50))
}

###################
# Main
#
dt1 <- readModel("gram1.txt",1)
dt2 <- readModel("gram2.txt",2)
dt3 <- readModel("gram3.txt",3)
dt4 <- readModel("gram4.txt",4)
l <- list(dt1, dt2, dt3, dt4)

saveModel(dt1, "gram1c.txt")
saveModel(dt2, "gram2c.txt")
saveModel(dt3, "gram3c.txt")
saveModel(dt4, "gram4c.txt")

#print(getBackoffWeight(dt3,"source of many"))

dictionary <- fread(paste0("vocabulary.txt"), sep="\t", header=T, 
                    stringsAsFactors = F, encoding = "UTF-8")

dfResult <- predictNextWord("what do you")
print(dfResult)
