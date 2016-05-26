####################
# Directories
#
#Virtual machine RWeka
options(java.parameters = "-Xmx8048m" )

# Sets the default number of threads to use
options(mc.cores=1)

scriptDir <- sys.frame(1)$ofile

PROJECTDIR = normalizePath(sprintf("%s/..", dirname(scriptDir)))
if (grepl("setenv.R$",scriptDir))
    PROJECTDIR = normalizePath(dirname(scriptDir))

DATADIR    = normalizePath(sprintf("%s/%s", PROJECTDIR, "../data"))
SAMPLEDIR  = normalizePath(sprintf("%s/%s", PROJECTDIR, "../sample"))
CLEANDIR   = normalizePath(sprintf("%s/%s", PROJECTDIR, "../clean"))

dir.create(DATADIR, showWarnings = F)
dir.create(SAMPLEDIR, showWarnings = F)
dir.create(CLEANDIR, showWarnings = F)

####################
# General constants
#
LANGUAGES  = c("en_US")
SOURCES    = c("twitter", "news", "blogs")

rm(scriptDir)
