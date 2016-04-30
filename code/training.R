library(dplyr)
library(data.table)
require(RWeka)

###################
# Environment
#
source("code/config.R")
source("code/constants.R")

###################
# Implementation
#
#---------------------------------------------------
# Read sentences from a text file 
#---------------------------------------------------
# param fileName  : the source text file
# return a data frame of sentences and words count
#
readSample <- function(fileName) {
    cat("Reading ", fileName,"\n")
    df <- tbl_df(read.table(fileName,allowEscapes = T, sep="|", stringsAsFactors = F)) %>%
        rename(text=V1)
    df$text <- sapply(df$text, function(s) sprintf("<s> %s </s>",s))
    
    return(df)
}

#---------------------------------------------------
# Build a list of n-gram frequency tables
#---------------------------------------------------
# param sentences   : a character vector
#       ngramOrders : a numeric vector
# return a list of data tables
#
text2ngram <- function(sentences, ngramOrders=c(1)) {
    ngramList <- list()
    for (order in ngramOrders) {
        ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = order, max = order))
        ngrams <- as.data.frame(table(as.vector(unlist(lapply(sentences, ngramTokenizer)))))
        names(ngrams)[1] <- "token"
        ngramList[[sprintf("%d-gram",order)]] <- data.table(ngrams, key = "token")
    }
    return(ngramList)
}

#---------------------------------------------------
# Build a list of frequency of frequency tables
#---------------------------------------------------
# param orderList: a list of n-gram frequency tables
# return a list of data tables
#
count2FreqCount <- function(orderList) {
    frequencyList <- list()
    for (n in names(orderList)) {
         frequency <- as.data.frame(table(l[[n]]$Freq))
         names(frequency) <- c("frequency","frequencyCount")
         frequencyList[[n]] <- data.table(frequency, key = "frequency")
    }
    return(frequencyList)
}

###################
# Main
#
#Read cleaned files
sentences <- NULL
for (src in SOURCES) {
    fileName <- sprintf("%s.%s.txt", "en_US", src)
    srcFile <- sprintf("%s/%s/%s", CLEANDIR, lang, fileName)
    if (is.null(sentences))
        sentences <- readSample(srcFile)
    else
        bind_rows(sentences,readSample(srcFile))
}

#Build ngram frequency table
ngramCount <- text2ngram(sentences, ngramOrders = c(1,2,3))

#Build frequency of frequency table
frequencyCount <- count2FreqCount(ngramCount)


rm(fileName, srcFile)