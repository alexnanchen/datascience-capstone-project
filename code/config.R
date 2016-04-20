####################
# Directories
#
scriptDir <- sys.frame(1)$ofile

PROJECTDIR = normalizePath(sprintf("%s/..", dirname(scriptDir)))
if (grepl("setenv.R$",scriptDir))
    PROJECTDIR = normalizePath(dirname(scriptDir))

DATADIR    = normalizePath(sprintf("%s/%s", PROJECTDIR, "../data"))
SAMPLEDIR  = normalizePath(sprintf("%s/%s", PROJECTDIR, "../sample"))

####################
# General constants
#
LANGUAGES  = c("en_US", "de_DE", "fi_FI", "ru_RU")
SOURCES    = c("twitter", "news", "blogs")

rm(scriptDir)
