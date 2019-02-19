# Duration of smoking (only for current and former Smokers)

# Duration of smoking for current smokers
smokingDurationCurrent <- function (obs) {
  ageAtStart <- startAge(obs)
  
  duration <- convertStringToNumber(obs["AgeAtBloodSample"]) - ageAtStart

  return(duration)
}

# Duration of smoking for formers smokers
smokingDurationFormer <- function (obs) {
  ageAtStart <- startAge(obs)
  ageAtStop <- stopAge(obs)

  return(ageAtStop - ageAtStart)
}

calculateSmokingDuration <- function(obs) {
  smokingStatus <- obs["SmokingStatus"]

  if (is.na(smokingStatus)) {
    return(NA)
  } else if (smokingStatus == "Current") {
    return(smokingDurationCurrent(obs))
  } else if (smokingStatus == "Former") {
    return(smokingDurationFormer(obs))
  } else {
    return(NA)
  }
}

smokingDuration <- function (women) {
  women$SmokingDuration <- apply(women, 1, calculateSmokingDuration)
  
  return(women)
}
