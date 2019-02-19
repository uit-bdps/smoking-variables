# Utility functions

# If the input is a character string, then trim it and convert to number, or 
# else return input as it is
convertStringToNumber <- function(numberStr) {
  if (is.character(numberStr)) {
    return(as.numeric(trim(numberStr)))
  }

  return(numberStr)
}

# For an integer, use the last two numbers as the year and make it four digits
extractYearFromIntegerDate <- function(intDate) {
  # Modulo (%%) operator to extract the last two digits
  year <- intDate %% 100

  # NOWAC participants not born before 1943
  if (year >= 43) {
    year <- year + 1900
  } else {
    year <- year + 2000
  }

  return(year)
}

# Find average of values given a vector of column names. Columns containing NA will be skipped.
# Note: 98 has a special meaning for smoking intensity options. Be aware that we are ignoring 
# these values.
calculateAverageOption <- function(obs, colNames) {
  if (length(colNames) < 1)
    return(NA)

  dividend <- 0
  divisor <- 0

  for (colName in colNames) {
    option <- obs[colName]

    if (!is.na(option) & !is.null(option)) {
      option <- convertStringToNumber(option)

      if (option != 98) {
        divisor <- divisor + 1
        dividend <- dividend + option
      }
    }
  }

  if (divisor != 0) {
    return(dividend / divisor)
  } else {
    return(NA)
  }
}

estimateStartAndStopAgeFromSpecifiedIntervals <- function(obs, colNames, baseAge, intervalWidth) {
  estimatedStart <- NA
  estimatedEnd <- NA

  i <- 0
  for (colName in colNames) {
    colValue <- convertStringToNumber(obs[colName])

    if (!is.na(colValue)) {
      if (is.na(estimatedStart)) {
        # First interval after NA is estimated Start age 
        # The average age start at "baseAge" and increase by "intervalWidth" for each interval
        estimatedStart <- baseAge + i
      }
    } else if (!is.na(estimatedStart)) {
      # The loop continues here, and the previous interval before new NA is estimated stop age.
      estimatedEnd <- baseAge + (i - intervalWidth)

      # Jump out of loop, we have found the value for start and stop age
      break
    }

    i <- i + intervalWidth
  }

  return(c(estimatedStart, estimatedEnd))
}

# Estimate start age from intervals
estimateStartAndStopAgeFromIntervals <- function(obs) {
  if (obs["ClosestQuest"] != "x") {
    # yROYKANT1-yROYKANT6
    res <- estimateStartAndStopAgeFromSpecifiedIntervals(obs, sprintf("yROYKANT%d", seq(1:6)), 14.5, 10)

    if (!is.na(res[1]) & !is.na(res[2])) {
      return(res)
    }
  }


  # ROK1-ROK8
  res <- estimateStartAndStopAgeFromSpecifiedIntervals(obs, sprintf("ROK%d", seq(1:8)), 12, 5)

  if (!is.na(res[1])) {
    return(res)
  }

  # ROYKANT1019-ROYKANT55MM
  res <- estimateStartAndStopAgeFromSpecifiedIntervals(obs, c("ROYKANT1019", "ROYKANT2029", "ROYKANT3039", "ROYKANT4049", "ROYKANT50MM"), 14.5, 10)

  if (!is.na(res[1])) {
    return(res)
  }

  # ROYKANT1-ROYKANT6
  res <- estimateStartAndStopAgeFromSpecifiedIntervals(obs, sprintf("ROYKANT%d", seq(1:6)), 14.5, 10)

  return(res)
}

estimateStartAgeFromIntervals <- function(obs) {
  return(estimateStartAndStopAgeFromIntervals(obs)[1])
}

estimateStopAgeFromIntervals <- function(obs) {
  return(estimateStartAndStopAgeFromIntervals(obs)[2])
}

# Age at start. Calculates an average if several different answers have been given
# for one woman.
startAge <- function(obs) {

  if (obs["ClosestQuest"] == "z")
    colNames <- c("SIGALDER", "YSIGALDER", "YSIGALDERB", "ZSIGALDER")
  else if (obs["ClosestQuest"] == "y")
    colNames <- c("SIGALDER", "YSIGALDER", "YSIGALDERB")
  else
    colNames <- c("SIGALDER")

  ageAtStart <- calculateAverageOption(obs, colNames)

  # Even if the closest questionnaire is x or y, we sometimes have a start age in z.
  # If we are missing the value for x or y, we can use ZSIGALDER if available.
  # As long as the start age is less than the blood sample date, or else it doesn't make sense.

  zAge <- obs["ZSIGALDER"]

  if (is.na(ageAtStart) & !is.na(zAge)) {
    zAge <- convertStringToNumber(zAge)
    ageAtBloodSample <- convertStringToNumber(obs["AgeAtBloodSample"])

    if (zAge < ageAtBloodSample)
      ageAtStart <- zAge
  }

  # If it is still not available (NA), inspect the smoking intervals/timeline
  if (is.na(ageAtStart))
    ageAtStart <- estimateStartAgeFromIntervals(obs)

  return(ageAtStart)
}

# Age at stop.  Calculates an average if several different answers have been given 
# for one woman.
stopAge <- function(obs) {

  if (obs["ClosestQuest"] == "z")
    colNames <- c("yROYKSTOP", "YRALDSLUTT", "ZROYKSTOP", "ZRALDSLUTT")
  else if (obs["ClosestQuest"] == "y")
    colNames <- c("yROYKSTOP", "YRALDSLUTT")
  else
    colNames <- character()

  ageAtStop <- calculateAverageOption(obs, colNames)

  # Even if the closest questionnaire is x or y, we sometimes have a stop age in z.
  # If we are missing the value for x or y, we can use "ZROYKSTOP" or "ZRALDSLUTT" if available.
  # As long as the stop age is less than the blood sample date, or else it doesn't make sense.

  colNames <- c("ZROYKSTOP", "ZRALDSLUTT")
  zAge <- calculateAverageOption(obs, colNames)

  if (is.na(ageAtStop) & !is.na(zAge)) {
    zAge <- convertStringToNumber(zAge)
    ageAtBloodSample <- convertStringToNumber(obs["AgeAtBloodSample"])

    if (zAge < ageAtBloodSample)
      ageAtStop <- zAge
  }

  # If it is still not available (NA), inspect the smoking intervals/timeline
  if (is.na(ageAtStop))
    ageAtStop <- estimateStopAgeFromIntervals(obs)

  return(ageAtStop)
}
