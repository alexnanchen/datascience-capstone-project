########################
# Environment settings
#
#Where setenv is located
scriptDir <- normalizePath(dirname(sys.frame(1)$ofile))
codeDir <- sprintf("%s/code", scriptDir)

#Library search path
.libPaths(c(.libPaths(),codeDir) )

#PATH variable
Sys.setenv(PATH=sprintf("%s:%s:%s", Sys.getenv()[["PATH"]], scriptDir,codeDir))

#Working directory git root
setwd(scriptDir)

#Some configuration
source("code/config.R")

rm(scriptDir, codeDir)
