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

  if (!is.na(ageAtStop) && !is.na(ageAtStart) && ageAtStop <= ageAtStart) {
    # There are inconsistencies in the data for this woman, and in this case the duration cannot be estimated.
    return(NA)
  }

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
    return(0)
  }
}

smokingDuration <- function (women) {
  women$SmokingDuration <- apply(women, 1, calculateSmokingDuration)
  
  return(women)
}
