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
    df$text <- sapply(df$text, function(s) sprintf("%s",s))
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
                  dplyr::filter(freq>minC)
        
        #Add to list
        ngramList[[sprintf("%d-gram",order)]] <- data.table(ngrams)
    }
    return(ngramList)
}

#-----------------------------------------------------
# Update unknown words with <unk> token
#-----------------------------------------------------
# param gram1     : a list of unique words
#       sentences : a list of sentences
# return a list of sentences with <unk> tokens
#
updateUnknownWords <- function(gram1, sentences) {
    #Index on unique word list
    setkey(gram1,word1)
    
    #Add sentence separator
    if (is.na(gram1[enc2utf8("dotsep")]$freq))
        gram1 <- rbind(list(enc2utf8("dotsep"),100), gram1)
    
    #Join text for faster processing
    print("Joining sentences")
    strText <- paste(sentences$text, collapse = " dotsep ")
    
    #Make a table
    print("Making a data table")
    dtText <- data.table(strsplit(strText," ")[[1]])
    dtText$seq <- seq(1, nrow(dtText))
    setkey(dtText,V1)
    
    #Join tables to get unknown words indices
    print("Merging unigram table and text words table")
    dtText <- suppressWarnings(merge(dtText, gram1, by.x="V1", 
                                     by.y = "word1", all.x=T))
    print("Setting unknown words")
    dtText$V1[which(is.na(dtText$freq))] <- "<unk>"
    
    #Re order and join words
    print("Reorder words and separate into sentences")
    setkey(dtText,seq)
    print(dtText)
    strText <- paste(dtText$V1, collapse=" ")
    
    #Make a data frame
    print("Make a data frame")
    df <- tbl_df(data.frame(text=(strsplit(strText, " dotsep ")[[1]]),
                 stringsAsFactors=F))
    Encoding(df$text) <- "UTF-8"
    return(df)
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
#Read cleaned files
sentences <- NULL
for (src in SOURCES) {
    fileName <- sprintf("%s.%s.txt", "en_US", src)
    srcFile <- sprintf("%s/%s/%s", CLEANDIR, "en_US", fileName)
    if (is.null(sentences)) {
        sentences <- readSample(srcFile)
    }
    else
        sentences <- bind_rows(sentences,readSample(srcFile))
}

#Build ngram frequency table
cat("N-gram extraction\n")
print("  --> Update unknown words")
gram1 <- text2ngram(sentences, ngramOrders = c(1), minCounts = c(3))[[1]]
writeCounts(gram1,"counts1.txt")

readline("Please check 'counts1.txt' file ")
gram1 <- data.table(read.table("counts1.txt", header=T, allowEscapes = T, sep="\t",
                    stringsAsFactors = F))

print(head(gram1))

#Obfuscate unknown words
sentences <- updateUnknownWords(gram1, sentences)

#Save sentences
write.table(sentences, "training.txt", quote=F, sep="\t", row.names=F,
            col.names=F, fileEncoding = "UTF-8")
writeCounts(gram1, "counts1.txt")

#Clean some memory
rmobj("gram1"); runGC()

cat("Re-extract ngrams\n")
ngramList <- text2ngram(sentences, ngramOrders = c(1,2,3,4), minCounts = c(3,2,1,0))
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
