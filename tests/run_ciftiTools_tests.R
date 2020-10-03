# Build --> Install and Restart
library(testthat)
ciftiTools.setOption('wb_path', '../workbench')
#ciftiTools.setOption('suppress_msgs', FALSE)
source("tests/testthat/test-reading.R")
source("tests/testthat/test-writing.R")
source("tests/testthat/test-resampling.R")
source("tests/testthat/test-misc.R")
