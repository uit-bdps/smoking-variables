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

test_that("Test calculateAverageOption(vector, names)", {
  vector <- c("0", "2", "3", "98", NA)
  names(vector) <- c("a", "b", "c", "d", "e")

  result <- calculateAverageOption(vector, names(vector))

  expect_equal(result, 2.5)
})


# estimateStartAndStopAgeFromIntervals

test_that("Test estimateStartAndStopAgeFromIntervals(vector) for ROK1 to ROK8", {
  vector <- c("z", NA, "0", "2", "2", "2", "0", NA, NA)
  names(vector) <- c("ClosestQuest", sprintf("ROK%d", seq(1:8)))

  result <- estimateStartAndStopAgeFromIntervals(vector)

  expect_equal(result[1], 22.5)
  expect_equal(result[2], 34)
})

test_that("Test estimateStartAndStopAgeFromIntervals(vector) for ROYKANT1014 to ROYKANT50MM", {
  vector <- c("z", NA, "3", "2", "2", "2", NA)
  names(vector) <- c("ClosestQuest", c("ROYKANT1014", "ROYKANT1519", "ROYKANT2029", "ROYKANT3039", "ROYKANT4049", "ROYKANT50MM"))

  result <- estimateStartAndStopAgeFromIntervals(vector)

  expect_equal(result[1], 17.5)
  expect_equal(result[2], 49)
})

test_that("Test estimateStartAndStopAgeFromIntervals(vector) for NA-only ROYKANT1014 to ROYKANT50MM", {
  vector <- c("x", NA, NA, NA, NA, NA, NA)
  names(vector) <- c("ClosestQuest", c("ROYKANT1014", "ROYKANT1519", "ROYKANT2029", "ROYKANT3039", "ROYKANT4049", "ROYKANT50MM"))

  result <- estimateStartAndStopAgeFromIntervals(vector)

  expect_equal(result[1], NA)
  expect_equal(result[2], NA)
})

test_that("Test estimateStartAndStopAgeFromIntervals(vector) for 0-only ROYKANT1014 to ROYKANT50MM", {
  vector <- c("x", 0, 0, 0, 0, 0, 0)
  names(vector) <- c("ClosestQuest", c("ROYKANT1014", "ROYKANT1519", "ROYKANT2029", "ROYKANT3039", "ROYKANT4049", "ROYKANT50MM"))

  result <- estimateStartAndStopAgeFromIntervals(vector)

  expect_equal(result[1], NA)
  expect_equal(result[2], NA)
})

test_that("Test estimateStartAndStopAgeFromIntervals(vector) for yROKANT1 to yROKANT6", {
  vector <- c("y", NA, "1", "2", "3", NA, NA)
  names(vector) <- c("ClosestQuest", sprintf("yROYKANT%d", seq(1:6)))

  result <- estimateStartAndStopAgeFromIntervals(vector)

  expect_equal(result[1], 25)
  expect_equal(result[2], 49)
})

test_that("Test estimateStartAndStopAgeFromIntervals(vector) for ROKANT1 to ROKANT6", {
  vector <- c("x", NA, "1", "2", "3", NA, NA)
  names(vector) <- c("ClosestQuest", sprintf("ROYKANT%d", seq(1:6)))

  result <- estimateStartAndStopAgeFromIntervals(vector)

  expect_equal(result[1], 25)
  expect_equal(result[2], 49)
})

# Start Age

test_that("Test startAge(vector) for ROKANT1 to ROKANT6 only", {
  vector <- c("x", NA, "1", "2", "3", NA, NA)
  names(vector) <- c("ClosestQuest", sprintf("ROYKANT%d", seq(1:6)))

  result <- startAge(vector)

  expect_equal(result, 25)
})

# Stop Age

test_that("Test stopAge(vector) for ROKANT1 to ROKANT6 only", {
  vector <- c("x", NA, "1", "2", "3", NA, NA)
  names(vector) <- c("ClosestQuest", sprintf("ROYKANT%d", seq(1:6)))

  result <- stopAge(vector)

  expect_equal(result, 49)
})

