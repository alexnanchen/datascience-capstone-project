#-----------------------------------------------------
# Utility function to remove an object
#-----------------------------------------------------
rmobj <- function(obj) {
    if (exists(obj))
        rm(obj)
}

#-----------------------------------------------------
# Interpolate with lower probability
#-----------------------------------------------------
# MITLM interpolate probabilities.
# Can be used to check that not interpolated values
# match interpolated values.
# see https://github.com/mitlm/mitlm/blob/master/src/KneserNeySmoothing.cpp
interpolate <- function(highestProb, lowerProb, lowerBow) {
    pLower <- 10^(lowerBow+lowerProb)
    pHigher <- 10^highestProb
    return(log10(pHigher+pLower))
}
