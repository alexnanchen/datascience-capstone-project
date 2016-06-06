library(dplyr)
library(data.table)
library(hashFunction)
library(testit)
library(bit64)

###################
# Environment
#
source("code/constants.R")
source("code/mod.R")
source("code/lm.R")

###################
# Implementation
#
compressModels <- function() {
    cat("Re compress files")
    dt1 <<- readModel("gram1_pruned.txt",1,T)
    dt2 <<- readModel("gram2_pruned.txt",2,T)
    dt3 <<- readModel("gram3_pruned.txt",3,T)
    dt4 <<- readModel("gram4_pruned.txt",4,T)
    
    saveModel(dt1, "gram1e.txt")
    saveModel(dt2, "gram2e.txt")
    saveModel(dt3, "gram3e.txt")
    saveModel(dt4, "gram4e.txt")
}

###################
# Main
#
#logValue <- getNgramLog(c("how", "are", "you"), 3, model, 4)
#print(logValue)
#strSentence <- "join hands to create a safer environment"
#strSentence <- "How are"
#ret <- getSentenceLog(strSentence, model, dictionary, 4)

dt1<-NULL
dt2<-NULL
dt3<-NULL
dt4<-NULL

#User check
if (file.exists("gram1e.txt")) {
    resp <- readline("Compress version exist, recompress (y|n)")
    if (resp == 'y')
        compressModels()
} else
    compressModels()

#Compress model loading
dt1 <- readCompressed("gram1e.txt")
dt1$stats <- as.numeric(dt1$stats)
dt2 <- readCompressed("gram2e.txt")
dt2$stats <- as.numeric(dt2$stats)
dt3 <- readCompressed("gram3e.txt")
dt3$stats <- as.numeric(dt3$stats)
dt4 <- readCompressed("gram4e.txt")
dt4$stats <- as.numeric(dt4$stats)

#One table
model = rbindlist(list(dt1,dt2,dt3,dt4))

#Ordered index
setkey(model,ngram)

#Some data
dictionary <- data.table(read.table("vocabulary.txt", header=T, allowEscapes = T, sep="\t",
                               stringsAsFactors = F, encoding="UTF-8"))

testFile <- sprintf("%s/en_US/en_US_twitter_test.txt", CLEANDIR)
sentences <- read.table(testFile, allowEscapes = T, sep="|", header=F, 
                stringsAsFactors = F, encoding = "UTF-8", col.names = c("text"))

#sentences = data.table(text="roy halladay said he began to get")

#Unknown words replacement as a batch
sentences <- updateUnknownWords(dictionary, sentences)

#Store for comparison with other toolkits
write.table(sentences, "testing.txt", quote=F, row.names = F, col.names = F,
            fileEncoding = "UTF-8")

#Main work
cat("Computing log probabilities per sentences\n")
totalLog <- 0; nbw <- 0;oov <- 0
count <- 0
for (s in tail(sentences$text, n=1000)) {
    ret <- getSentenceLog(s, model, dictionary, 4, F)
    #cat(ret$totalLog, s,"\n")
    totalLog <- totalLog + round(ret$totalLog,3)
    nbw <- nbw + ret$nbw
    oov <- oov + ret$oov
    count <- count + 1
    if (count %% 100 == 0)
        cat("Done ", count, "sentences\n")
}

ppl <- 10^(-totalLog /(nbw - oov + count))
cat("Total log:", totalLog, "- nb words:", nbw, "- oov:", oov, "nb sentences:", count,
    "- ppl:", ppl, "\n")
