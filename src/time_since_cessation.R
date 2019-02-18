# Time since cessation (TSC) (only needed for former smokers)

calculateTimeSinceCessation <- function (obs) {
  ageAtStop <- stopAge(obs)
  
  tsc <- convertStringToNumber(obs["AgeAtBloodSample"]) - ageAtStop
  
  return(tsc)
}

calculateTSC <- function (obs) {
  smokingStatus <- obs["SmokingStatus"]
  
  if (is.na(smokingStatus)) return(NA)
  else if (smokingStatus == "Former") return(calculateTimeSinceCessation(obs))
  
  # Just to check if there are any mistakes in the "Current" and "Never"-status. 
  # If the answer is 0 or NA, then it is correct.
  
  else if (smokingStatus == "Current") return(calculateTimeSinceCessation(obs)) 
  else if (smokingStatus == "Never") return(calculateTimeSinceCessation(obs))   
  
  else return(NA)
}

timeSinceCessation <- function (women) {
  women$TSC <- apply(women, 1, calculateTSC)
  
  return(women)
}
