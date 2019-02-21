# Intensity (average number of cigarettes smoked per day)

optionToIntensityVector <- c(0, 2.5, 7, 12, 17, 22, 27)

# Option for X
calculateOptionX <- function(obs) {
  colNames <- sprintf("ROK%d", seq(1:8))
  result <- calculateAverageOption(obs, colNames)

  if (!is.na(result[1])) {
    return(result)
  }
  
  colNames <- c("ROYKANT1019", "ROYKANT2029", "ROYKANT3039", "ROYKANT4049", "ROYKANT50MM")
  result <- calculateAverageOption(obs, colNames)
  
  if (!is.na(result[1])) {
    return(result)
  } 
  
  colNames <- c("ROYKANT1", "ROYKANT2", "ROYKANT3", "ROYKANT4", "ROYKANT5", "ROYKANT6")
  result <- calculateAverageOption(obs, colNames)
  
  return(result)
}

# Option for Y
calculateOptionY <- function (obs) {
  resultX <- calculateOptionX(obs)
  optionX <- resultX[1]
  weightX <- resultX[2]
  
  colNames <- c("yROYKANT1", "yROYKANT2", "yROYKANT3", "yROYKANT4", "yROYKANT5", "yROYKANT6")
  resultY <- calculateAverageOption(obs, colNames)
  optionY <- resultY[1]
  weightY <- resultY[2]

  if (!is.na(optionY)) {
    if (!is.na(optionX)) {
      avgWeighted <- ((optionX * weightX) + (optionY * weightY)) / (weightX + weightY)

      # This is so calculation for z-option will know number of 10 year intervals that was used (average). X and Y intervals should be almost overlapping, thus the add and divide by two.
      avgWeightForNextStep <- (weightX + weightY) / 2 

      return(c(avgWeighted, avgWeightForNextStep))
    } else {
      return(resultY)
    }
  } 
  
  return(resultX)
}

# Option for Z
calculateOptionZ <- function (obs) {
  resultY <- calculateOptionY(obs)
  optionY <- resultY[1]
  weightY <- resultY[2]

  optionZ <- NA
  weightZ <- NA

  last5years <- convertStringToNumber(obs["ZROKSIST5"])
  last8years <- convertStringToNumber(obs["ZROKSIST8"])

  if (!is.na(last5years) & (last5years != 0)) {
    optionZ <- last5years
    weightZ <- 5 / 10 # Divide by ten because the weight from Y is 1 per 10 year interval
  } else if (!is.na(last8years) & (last8years != 0)) {
    optionZ <- last8years
    weightZ <- 8 / 10 
  }
  
  if (!is.na(optionZ)) {
    if (!is.na(optionY)) {
      avgIntensity <- ((optionY * weightY) + (optionZ * weightZ)) / (weightY + weightZ)
      return(avgIntensity) 
    } else {
      return(optionZ)
    }
  }
  
  return(optionY)
}

calculateIntensity <- function (obs) {
  option <- NA
  
  if (obs["ClosestQuest"] == "x") {
    option <- calculateOptionX(obs)[1] # calculateOptionX returns a vector with avg. option and a weight
  } else if (obs["ClosestQuest"] == "y") {
    option <- calculateOptionY(obs)[1] # calculateOptionY returns a vector with avg. option and a weight
  } else {
    option <- calculateOptionZ(obs) # calculateOptionZ returns a single value
  }
  
  option <- trunc(option + 0.5) # Use trunc instead of round, because round rounds 0.5 to even.
  
  return(optionToIntensityVector[option + 1])
}

calculateOption1019 <- function (obs) {
  colNames <- c("ROYKANT1014", "ROYKANT1519")
  option <- calculateAverageOption(obs, colNames)[1]
  
  if (!is.na(option))
    return(option)
  
  return(NA)
}

zeroIntensityForNeverSmokers <- function (woman) {
  smokingStatus <- woman["SmokingStatus"]

  if (!is.na(smokingStatus) & smokingStatus == "Never") {
    return(0)
  } else {
    return(woman["Intensity"])
  }
}

setNeverSmokerIntensitiesToZero <- function (women) {
  women$Intensity <- apply(women, 1, zeroIntensityForNeverSmokers)
  return(women)
}

smokingIntensity <- function (women) {
  women$ROYKANT1019 <- apply(women, 1, calculateOption1019)
  women$Intensity <- apply(women, 1, calculateIntensity)
  
  return(women)
}
