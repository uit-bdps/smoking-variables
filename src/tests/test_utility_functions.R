if (!require(testthat)) {
  install.packages(testthat)
  library(testthat)
}

source("../utility_functions.R")

# convertStringToNumber

test_that("Test convertStringToNumber(\" 1 \")", {
  result <- convertStringToNumber(" 1 ")
  
  expect_type(result, "double")
  expect_equal(result, 1)
})

test_that("Test convertStringToNumber(1)", {
  result <- convertStringToNumber(1)
  
  expect_type(result, "double")
  expect_equal(result, 1)
})

test_that("Test convertStringToNumber(NA)", {
  result <- convertStringToNumber(NA)
  
  expect_equal(is.na(result), TRUE)
})

# extractYearFromIntegerDate

test_that("Test extractYearFromIntegerDate(43)", {
  result <- extractYearFromIntegerDate(43)
  
  expect_equal(result, 1943)
})

test_that("Test extractYearFromIntegerDate(42)", {
  result <- extractYearFromIntegerDate(42)
  
  expect_equal(result, 2042)
})

test_that("Test extractYearFromIntegerDate(00)", {
  result <- extractYearFromIntegerDate(00)
  
  expect_equal(result, 2000)
})

test_that("Test extractYearFromIntegerDate(99)", {
  result <- extractYearFromIntegerDate(99)
  
  expect_equal(result, 1999)
})

# calculateAverageOption

test_that("Test calculateAverageOption(vector, names)", {
  vector <- c("1", "2", "3", "98", NA)
  names(vector) <- c("a", "b", "c", "d", "e")
  
  result <- calculateAverageOption(vector, names(vector))
  
  expect_equal(result, 2)
})

# estimateStartAndStopAgeFromIntervals

test_that("Test estimateStartAndStopAgeFromIntervals(vector) for yROKANT1 to yROKANT6", {
  vector <- c("y", NA, "1", "2", "3", NA, NA)
  names(vector) <- c("ClosestQuest", sprintf("yROYKANT%d", seq(1:6)))

  result <- estimateStartAndStopAgeFromIntervals(vector)


  expect_equal(result[1], 24.5)
  expect_equal(result[2], 44.5)
})


