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
    cat("Reading ", fileName)
    df <- tbl_df(read.table(fileName,allowEscapes = T)) %>%
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
# Filter data
#---------------------------------------------------
# Filter the given data frame using the following
# heuristic:
#    - maximum number of digit groups
# return a filtered data frame
#
filterSentences <- function(df) {
    
    
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
        strText <- gsub(v[1], v[2], strText, perl = T)
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
cleanSentences <- function(df) {
    #Join text for faster processing
    strText <- paste(df$text, collapse = " $$dot$$ ")
    
    #Tab, carriage return and new lines replacements
    strText <- gsub("\t|\r|\n", "$$dot$$", strText, perl = T)
    
    #-----------------------------
    # Word based replacement
    #
    #http://www.webopedia.com/quick_ref/Twitter_Dictionary_Guide.as
    #Twitter words replacement
    
    
    #Offensive words replacement
    
    #-----------------------------
    # Character based replacement
    #
    #Emoticons replacement
    
    #Control characters replacement
    
    #Utf-8 characters replacement
    strText <- mapUtf8Characters(strText, UTF8MAP)
    
    #-----------------------------
    # Other replacement
    #
    c <- VCorpus(VectorSource(strText))
    c <- tm_map(c, tolower)
    c <- tm_map(c, removeNumbers)
    #c <- tm_map(c, removePunctuation)
    #c <- tm_map(c, removeWords, stopwords("english"))
    #c <- tm_map(c, stemDocument, "english")
    c <- tm_map(c, stripWhitespace)
    c <- tm_map(c, PlainTextDocument)
    
    strText <- as.character(c[[1]])
    
    print(strText)
    
    #Segment text using dots
    #Set new dataframe
}



###################
# Main
#
samples <- readAllSamples()
#test <- as.character("\u03B1")
#mapUtf8Characters(test,UTF8MAP)
#cleanSentences(samples$en_US$twitter)
