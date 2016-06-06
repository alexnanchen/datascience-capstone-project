library(dplyr)
library(data.table)
library(hashFunction)
library(testit)
library(tidyr)

###################
# Environment
#
source("code/mod.R")
source("code/prediction.R")

###################
# Main
#
dt1 <- readModel("gram1_pruned.txt",1)
dt2 <- readModel("gram2_pruned.txt",2)
dt3 <- readModel("gram3_pruned.txt",3)
dt4 <- readModel("gram4_pruned.txt",4)

saveModel(dt1, "gram1c.txt")
saveModel(dt2, "gram2c.txt")
saveModel(dt3, "gram3c.txt")
saveModel(dt4, "gram4c.txt")

l <- list(dt1, dt2, dt3, dt4)

#print(getBackoffWeight(dt3,"source of many"))

dictionary <- fread(paste0("vocabulary.txt"), sep="\t", header=T, 
                    stringsAsFactors = F, encoding = "UTF-8")

dfResult <- predictNextWord("what do you", dictionary)
print(head(dfResult, n=10))
