# Build --> Install and Restart
# [Edit this] path to the Workbench for your computer.
my_wb <- "../workbench"

library(testthat)
if (interactive()) { ciftiTools.setOption("wb_path", my_wb) }
tests_dir <- "testthat"
if (!endsWith(getwd(), "tests")) { tests_dir <- file.path("tests", tests_dir) }
source(file.path(tests_dir, "test-reading.R"))
source(file.path(tests_dir, "test-writing.R"))
source(file.path(tests_dir, "test-resampling.R"))
source(file.path(tests_dir, "test-plotting_surf.R"))
source(file.path(tests_dir, "test-misc.R"))
