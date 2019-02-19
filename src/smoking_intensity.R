# Intensity (average no. of cigarettes smoked per day)

optionToIntensityVector <- c(0, 2.5, 7, 12, 17, 22, 27)

# Option for X
calculateOptionX <- function (obs) {
  colNames <- sprintf("ROK%d", seq(1:8))
  option <- calculateAverageOption(obs, colNames)
  
  if (!is.na(option)) {
    return(option)
  }
  
  colNames <- c("ROYKANT1019", "ROYKANT2029", "ROYKANT3039", "ROYKANT4049", "ROYKANT50MM")
  option <- calculateAverageOption(obs, colNames)
  
  if (!is.na(option)) {
    return(option)
  } 
  
  colNames <- c("ROYKANT1", "ROYKANT2", "ROYKANT3", "ROYKANT4", "ROYKANT5", "ROYKANT6")
  option <- calculateAverageOption(obs, colNames)
  
  if (!is.na(option)) {
    return(option)
  } 
  
  return(NA)  
}

# Option for Y
calculateOptionY <- function (obs) {
  
  optionX <- calculateOptionX(obs)
  
  colNames <- c("yROYKANT1", "yROYKANT2", "yROYKANT3", "yROYKANT4", "yROYKANT5", "yROYKANT6")
  optionY <- calculateAverageOption(obs, colNames)
  
  if (!is.na(optionY)) {
    if (!is.na(optionX)) {
      return((optionX + optionY) / 2)
    } else {
      return(optionY)
    }
  } 
  
  return(optionX)
}

# Option for Z
calculateOptionZ <- function (obs) {
  
  optionY <- calculateOptionY(obs) #includes option for both x and y
  optionZ <- NA

  last5years <- convertStringToNumber(obs["ZROKSIST5"])
  last8years <- convertStringToNumber(obs["ZROKSIST8"])

  if (!is.na(last5years) & (last5years != 0)) {
    optionZ <- last5years
  } else if (!is.na(last8years) & (last8years != 0)) {
    optionZ <- last8years
  }
  
  if (!is.na(optionZ)) {
    if (!is.na(optionY)) {
      return((optionZ + optionY) / 2)
    } else {
      return(optionZ)
    }
  }
  
  return(optionY)
}

calculateIntensity <- function (obs) {
  option <- NA
  
  if (obs["ClosestQuest"] == "x") {
    option <- calculateOptionX(obs)
  } else if (obs["ClosestQuest"] == "y") {
    option <- calculateOptionY(obs)
  } else {
    option <- calculateOptionZ(obs)
  }
  
  option <- trunc(option + 0.5) # Use trunc instead of round, because round rounds 0.5 to even.
  
  return(optionToIntensityVector[option + 1])
}

calculateOption1019 <- function (obs){
  colNames <- c("ROYKANT1014", "ROYKANT1519")
  option <- calculateAverageOption(obs, colNames)
  
  if (!is.na(option))
    return(option)
  
  return(NA)
}

smokingIntensity <- function (women) {
  women$ROYKANT1019 <- apply(women, 1, calculateOption1019)
  women$Intensity <- apply(women, 1, calculateIntensity)
  
  return(women)
}
