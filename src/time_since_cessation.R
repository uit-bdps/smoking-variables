# Time since cessation (TSC) (only needed for former smokers)

calculateTimeSinceCessation <- function (obs) {
  ageAtStop <- stopAge(obs)
  
  tsc <- convertStringToNumber(obs["AgeAtBloodSample"]) - ageAtStop
  
  return(tsc)
}

calculateTSC <- function (obs) {
  smokingStatus <- obs["SmokingStatus"]
  
  if (is.na(smokingStatus)) {
    return(NA)
  } else if (smokingStatus == "Former") {
    return(calculateTimeSinceCessation(obs))
  } else {
    return(0)
  }
}

timeSinceCessation <- function (women) {
  women$TSC <- apply(women, 1, calculateTSC)
  
  return(women)
}
