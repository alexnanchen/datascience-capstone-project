library(dplyr)
library(data.table)
library(RWeka)
library(tidyr)

###################
# Environment
#
source("code/mod.R")
source("code/constants.R")
source("code/lm.R")

###################
# Implementation
#
#-----------------------------------------------------
# Read sentences from a text file 
#-----------------------------------------------------
# param fileName  : the source text file
# return a data frame of sentences and words count
#
readSample <- function(fileName) {
    cat("Reading ", fileName,"\n")
    df <- tbl_df(read.table(fileName,allowEscapes = T, sep="|", stringsAsFactors = F)) %>%
        rename(text=V1)
    df$text <- sapply(df$text, function(s) sprintf("<s> %s </s>",s))
    Encoding(df$text) <- "UTF-8"
    return(df)
}

#-----------------------------------------------------
# Build a list of n-gram frequency tables
#-----------------------------------------------------
# param sentences   : a character vector
#       ngramOrders : a numeric vector
#       minCounts   : discard ngram with count lower
#                     or equal
# return a list of data tables
#
text2ngram <- function(sentences, ngramOrders, minCounts) {
    ngramList <- list()
    for (i in seq(1, length(ngramOrders))) {
        order <- ngramOrders[i]
        minC <- minCounts[i]
        cat("Extracting", order, "grams\n")
        ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = order, max = order))
        #Build counts
        ngrams <- as.data.frame(table(as.vector(unlist(lapply(sentences, ngramTokenizer)))),
                                stringsAsFactors=F)
        
        names(ngrams)[1] <- "token"
        
        #Check all same order
        filteredIndices <- which(sapply(ngrams$token,function(s) {length(strsplit(s," ")[[1]])}!=order))
        if (length(filteredIndices) > 0)
            ngrams <- ngrams[-filteredIndices,]
    
        #Re-order
        ngrams <- dplyr::select(ngrams, token, freq=Freq)
        
        #Split token into words
        colNames <- sapply(seq(1,order), function(s) paste0("word",s))
        ngrams <- tidyr::separate(ngrams,"token",colNames," ") %>% 
                  dplyr::filter(freq>=minC)
        
        #Add to list
        ngramList[[sprintf("%d-gram",order)]] <- data.table(ngrams)
    }
    return(ngramList)
}

#-----------------------------------------------------
# Write counts for training to disk
#-----------------------------------------------------
# param ngram     : a count data table
#       fileName  : where to save to
# return a list of sentences with <unk> tokens
#
writeCounts <- function(ngram, fileName) {
    cat("Writing", nrow(ngram), "counts for", fileName, "\n")
    write.table(ngram, fileName, sep="\t", row.names = F, col.names = T, quote=T,
                fileEncoding = "UTF-8")
}

###################
# Main
#
MINVOCABULARYCOUNT = 3

#Read cleaned files
sentences <- NULL
for (src in SOURCES) {
    fileName <- sprintf("%s_%s_train.txt", "en_US", src)
    srcFile <- sprintf("%s/%s/%s", CLEANDIR, "en_US", fileName)
    if (is.null(sentences)) {
        sentences <- readSample(srcFile)
    }
    else
        sentences <- bind_rows(sentences,readSample(srcFile))
}

#Build ngram frequency table
cat("N-gram extraction\n")
print("  --> Vocabulary extraction")
gram1 <- text2ngram(sentences, ngramOrders = c(1), minCounts = c(MINVOCABULARYCOUNT))[[1]]
writeCounts(gram1,"vocabulary.txt")

#User check
readline("Please check 'vocabulary.txt' file and then press <enter>")
gram1 <- data.table(read.table("vocabulary.txt", header=T, allowEscapes = T, sep="\t",
                    stringsAsFactors = F))
print(head(gram1))

#Obfuscate unknown words
sentences <- updateUnknownWords(gram1, sentences)

#Save sentences
write.table(sentences, "training.txt", quote=F, sep="\t", row.names=F,
            col.names=F, fileEncoding = "UTF-8")

#Clean some memory
rmobj("gram1"); runGC()

cat("Re-extract ngrams on text with <unk> words\n")
#No token filtering here. One and two counts are needed
#for discount computation and smoothing
ngramList <- text2ngram(sentences, ngramOrders = c(1,2,3,4), minCounts = c(0,0,0,0))
gram1 <- ngramList[[1]]
gram2 <- ngramList[[2]]
gram3 <- ngramList[[3]]
gram4 <- ngramList[[4]]

#Models serialization
cat("Writing counts to disk\n")
writeCounts(gram1, "counts1.txt")
writeCounts(gram2, "counts2.txt")
writeCounts(gram3, "counts3.txt")
writeCounts(gram4, "counts4.txt")

rmobj("fileName")
rmobj("srcFile")
