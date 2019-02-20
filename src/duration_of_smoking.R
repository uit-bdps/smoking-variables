# Duration of smoking (only for current and former smokers)

# Duration of smoking for current smokers
smokingDurationCurrent <- function (obs) {
  ageAtStart <- startAge(obs)
  
  duration <- convertStringToNumber(obs["AgeAtBloodSample"]) - ageAtStart

  return(duration)
}

# Duration of smoking for formers smokers
smokingDurationFormer <- function (obs) {
  ageAtStart <- startAge(obs)
  ageAtBloodSample <- convertStringToNumber(obs["AgeAtBloodSample"])
  tsc <- convertStringToNumber(obs["TSC"])

  if (is.na(ageAtBloodSample) || is.na(ageAtStart) || is.na(tsc)) {
    # Missing values prevent duration from being estimated
    return(NA)
  }

  duration <- ageAtBloodSample - tsc - ageAtStart

  if (duration > 0) {
    return(duration)
  } else {
    return(NA)
  }
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
    return(0)
  }
}

smokingDuration <- function (women) {
  women$SmokingDuration <- apply(women, 1, calculateSmokingDuration)
  
  return(women)
}
