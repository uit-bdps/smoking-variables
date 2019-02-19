rm(list = ls())

if (!require(dplyr)) {
  install.packages(dplyr)
  library(dplyr)
}

if (!require(gdata)) {
  install.packages(gdata)
  library(gdata)
}

if (!require(testthat)) {
  install.packages(testthat)
  library(testthat)
} 

test_results <- test_dir("tests", reporter="summary")
