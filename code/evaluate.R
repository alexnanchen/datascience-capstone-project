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

dictionary <- fread(paste0("vocabulary.txt"), sep="\t", header=T,
                    stringsAsFactors = F, encoding = "UTF-8")

logValue <- getNgramLog(c("how", "are", "you"), 3, model, 4)
print(logValue)

#strSentence <- "join hands to create a safer environment"
strSentence <- "How are"
ret <- getSentenceLog(strSentence, model, dictionary, 4)

ppl1 <- 10^(-ret$totalLog /(ret$nbw - ret$oov+1))
cat("Total log:", ret$totalLog, "- nb words:", ret$nbw, "- oov:", ret$oov, "- ppl1:", ppl1, "\n")
