rm(list=ls())

if (!require(testthat)) {
  install.packages(testthat)
  library(testthat)
} 

test_results <- test_dir("tests", reporter="summary")


