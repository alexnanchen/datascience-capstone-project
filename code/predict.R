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
dt1 <- readModel("gram1.txt",1)
dt2 <- readModel("gram2.txt",2)
dt3 <- readModel("gram3.txt",3)
dt4 <- readModel("gram4.txt",4)
l <- list(dt1, dt2, dt3, dt4)

saveModel(dt1, "gram1c.txt")
saveModel(dt2, "gram2c.txt")
saveModel(dt3, "gram3c.txt")
saveModel(dt4, "gram4c.txt")

#print(getBackoffWeight(dt3,"source of many"))

dfResult <- predict("what do you")
dfResult <- group_by(dfResult,word) %>% summarize(mean=mean(logprob), 
                maxorder=max(order)) %>% arrange(desc(maxorder), desc(mean))
print(dfResult)
