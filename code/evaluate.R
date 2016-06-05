library(dplyr)
library(data.table)
library(hashFunction)
library(testit)

###################
# Environment
#
source("code/lm.R")
source("code/constants.R")

###################
# Main
#
dt1 <- readModel("gram1.txt")
dt2 <- readModel("gram2.txt")
dt3 <- readModel("gram3.txt")
dt4 <- readModel("gram3.txt")

saveModel(dt1, "gram1e.txt")
saveModel(dt2, "gram2e.txt")
saveModel(dt3, "gram3e.txt")
saveModel(dt4, "gram4e.txt")

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

ppl1 <- 10^(-ret$totalLog /(ret$nbw - ret$oov))
cat("Total log:", ret$totalLog, "- nb words:", ret$nbw, "- oov:", ret$oov, "- ppl1:", ppl1, "\n")
