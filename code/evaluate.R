library(dplyr)
library(data.table)
library(hashFunction)
library(testit)

###################
# Environment
#
source("code/constants.R")
source("code/mod.R")
source("code/lm.R")

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

if (!file.exists("gram1e.txt")) {
    dt1 <- readModel("gram1.txt",1,T)
    dt2 <- readModel("gram2.txt",2,T)
    dt3 <- readModel("gram3.txt",3,T)
    dt4 <- readModel("gram4.txt",4,T)
    
    saveModel(dt1, "gram1e.txt")
    saveModel(dt2, "gram2e.txt")
    saveModel(dt3, "gram3e.txt")
    saveModel(dt4, "gram4e.txt")
} else {
    dt1 <- readCompressed("gram1e.txt")
    dt1$stats <- as.numeric(dt1$stats)
    dt2 <- readCompressed("gram2e.txt")
    dt2$stats <- as.numeric(dt2$stats)
    dt3 <- readCompressed("gram3e.txt")
    dt3$stats <- as.numeric(dt3$stats)
    dt4 <- readCompressed("gram4e.txt")
    dt4$stats <- as.numeric(dt4$stats)
}

#One table
model = rbindlist(list(dt1,dt2,dt3,dt4))

#Ordered index
setkey(model,ngram)

#Some data
dictionary <- data.table(read.table("vocabulary.txt", header=T, allowEscapes = T, sep="\t",
                               stringsAsFactors = F, encoding="UTF-8"))

testFile <- sprintf("%s/en_US/en_US_news_test.txt", CLEANDIR)
sentences <- read.table(testFile, allowEscapes = T, sep="|", header=F, 
                        stringsAsFactors = F, encoding = "UTF-8", col.names = c("text"))

#Unknown words replacement as a batch
sentences <- updateUnknownWords(dictionary, sentences)

#sentences = data.table(text="roy halladay said he began to get that certain feeling along")

#Main work
cat("Computing log probabilities per sentences\n")
totalLog <- 0; nbw <- 0;oov <- 0
count <- 0
for (s in tail(sentences$text, n=1000)) {
    ret <- getSentenceLog(s, model, dictionary, 4, F)
    #cat(ret$totalLog, s,"\n")
    totalLog <- totalLog + ret$totalLog
    nbw <- nbw + ret$nbw
    oov <- oov + ret$oov
    count <- count + 1
    if (count %% 100 == 0)
        cat("Done ", count, "sentences\n")
}

ppl1 <- 10^(-totalLog /(nbw - oov + count))
cat("Total log:", totalLog, "- nb words:", nbw, "- oov:", oov, "nb sentences:", count, 
    "- ppl1:", ppl1, "\n")

