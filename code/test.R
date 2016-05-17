###################
# Environment
#
source("code/lm.R")

###################
# Implementation
#
#-----------------------------------------------------
# Some basic checks
#-----------------------------------------------------
# Could be implemented as a unit test.
testAll <- function() {
    i1 <- -9.9999967; i2 <- -98.255557
    print("Packing ") 
    cat(" -->"); print(i1, digits=8)
    cat(" -->"); print(i2, digits=8)
    i <- stats2double(i1,i2); cat(" -->"); print(i, digits=20)
    print(object.size(i))
    cat("Unpacking", i, " ")
    print(double2stats(i), digits = 8)
    cat("Hashing 'Hello économie'", ngram2hash("Hello économie"))
}

###################
# Main
#
testAll()
