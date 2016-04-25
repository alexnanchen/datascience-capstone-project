library(dplyr)
library(tm)

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
             rename(text=V1, wordCount = V2)
    return(df)
}

#---------------------------------------------------
# Read all samples for all languages
#---------------------------------------------------
# return a list of languages containing a list of
#        samples
#
readAllSamples <- function() {
    ret <- list()
    for (lang in LANGUAGES) {
        dir.create(paste0(CLEANDIR,"/",lang), showWarnings = F)
        for (src in SOURCES) {
            #Input and output names
            fileName <- sprintf("%s.%s.txt", lang, src)
            srcFile <- sprintf("%s/%s/%s", SAMPLEDIR, lang, fileName)
            #Read sample sentences
            ret[[lang]][[src]] <- readSample(srcFile)
        }
    }
    return(ret)
}

#---------------------------------------------------
# Map some utf8 characters
#---------------------------------------------------
# param strText  : a character string
# return a character string
#
mapUtf8Characters <- function(strText, utf8List) {
    for (v in utf8List) {
        #print(sprintf("%s %s %s",v[1], v[2], v[3]))
        strText <- gsub(enc2utf8(v[1]), enc2utf8(v[2]), enc2utf8(strText), perl = T)
    }
    return(strText)
}

#---------------------------------------------------
# Clean data
#---------------------------------------------------
# Clean the given data frame using the following
# heuristic:
#    - Noise words filtering
#    - Character based normalization (i.e. control characters, utf-8 characters)
#    - Case normalization
#    - Punctuation removal
# return a filtered data frame
#
cleanSentences <- function(df, offensiveWords, destFile) {
    #Join text for faster processing
    strText <- paste(df$text, collapse = " dotsep ")
    
    #Tab, carriage return and new lines replacements
    strText <- gsub("\t|\r|\n", " dotsep ", strText, perl = T)
    
    #-----------------------------
    # Character based replacement
    #
    cat("  --> Character based replacement")
    #Emoticons replacement
    strText <- mapUtf8Characters(strText, EMOTICONSMAP)
    
    #Control characters replacement
    strText <- mapUtf8Characters(strText, CONTROLMAP)
    
    #Utf-8 characters replacement
    strText <- mapUtf8Characters(strText, UTF8MAP)
    
    #-----------------------------
    # Other replacement
    #
    cat("  --> Other replacement")
    c <- VCorpus(VectorSource(strText))
    c <- tm_map(c, tolower)
    c <- tm_map(c, removeNumbers)
    c <- tm_map(c, removePunctuation)
    c <- tm_map(c, stripWhitespace)
    c <- tm_map(c, PlainTextDocument)
    c <- tm_map(c, removeWords, offensiveWords)
 
    #Resegment into sentences   
    sentences <- strsplit(as.character(c[[1]]), " dotsep ")[[1]]
    
    #Write results
    cat("  --> Outputing to", destFile, "\n")
    write.table(sentences, destFile, sep="\t", row.names = F, col.names = F, quote=T)
}

###################
# Main
#
# To test that characters are substitued:
# mapUtf8Characters(as.character("\ab"),CONTROLMAP)
#
# Load data sample
samples <- readAllSamples()
offensiveWords <- read.csv("resources/offensive.csv",header=F,stringsAsFactors = F)

# Main processing
for (lang in LANGUAGES) {
    for (src in SOURCES) {
        #Output names
        fileName <- sprintf("%s.%s.txt", lang, src)
        destFile <- sprintf("%s/%s/%s", CLEANDIR, lang, fileName)
        
        #Data preparation
        cat("Cleaning ", fileName)
        cleanSentences(samples$en_US[[src]], offensiveWords$V1, destFile)
    }
} 

rm(fileName, destFile)
