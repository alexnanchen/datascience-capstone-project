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
            for (s in c("train", "dev", "test")) {
                #Input and output names
                fileName <- sprintf("%s_%s_%s.txt", lang, src, s)
                srcFile <- sprintf("%s/%s/%s", SAMPLEDIR, lang, fileName)
                #Read sample sentences
                ret[[lang]][[src]][[s]] <- readSample(srcFile)
            }
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
        print(sprintf("%s %s %s",v[1], v[2], v[3]))
        strText <- gsub(enc2utf8(v[1]), enc2utf8(v[2]), enc2utf8(strText), fixed = T)
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
    
    cat("  --> Extra tabs, carriage returns and new lines removal\n")
    
    #Tab, carriage return and new lines replacements
    strText <- gsub("\t|\r|\n", " dotsep ", strText, perl = T)
    
    #-----------------------------
    # Character based replacement
    #
    cat("  --> Character based replacement\n")
    #Emoticons replacement
    strText <- mapUtf8Characters(strText, EMOTICONSMAP)
    
    #Control characters replacement
    strText <- mapUtf8Characters(strText, CONTROLMAP)
    
    #Utf-8 characters replacement
    strText <- mapUtf8Characters(strText, UTF8MAP)
    
    #-----------------------------
    # Other replacement
    #
    cat("  --> Other replacement\n")
    c <- VCorpus(VectorSource(strText))
    print("To lower")
    c <- tm_map(c, tolower)
    print("removeNumbers")
    c <- tm_map(c, removeNumbers)
    print("Remove punctuation")
    c <- tm_map(c, removePunctuation)
    print("stripWhitespace")
    c <- tm_map(c, stripWhitespace)
    print("Plain text document")
    c <- tm_map(c, PlainTextDocument)
    print("Removing offensive words")
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
        for (s in c("train", "dev", "test")) {
            #Output names
            fileName <- sprintf("%s_%s_%s.txt", lang, src, s)
            destFile <- sprintf("%s/%s/%s", CLEANDIR, lang, fileName)
            
            #Data preparation
            cat("Cleaning ", fileName)
            cleanSentences(samples[[lang]][[src]][[s]], offensiveWords$V1, destFile)
        }
    }
} 

rm(fileName, destFile)
