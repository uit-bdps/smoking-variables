# Time since cessation (TSC) (only needed for former smokers)

reestimateStopAge <- function(ageAtStop, ageAtBloodSample) {
  stopAgeFloor <- (trunc(ageAtStop / 10) * 10)

  ageAtStop <- stopAgeFloor + ((ageAtBloodSample - stopAgeFloor) / 2)

  return(ageAtStop)
}

calculateTimeSinceCessation <- function (obs) {
  ageAtStop <- stopAge(obs)
  ageAtBloodSample <- convertStringToNumber(obs["AgeAtBloodSample"])

  if (!is.na(ageAtStop) && !is.na(ageAtBloodSample) && ageAtBloodSample <= ageAtStop) {
    # The ageAtStop was probably calculated from upper bound of age intervals.
    # If the woman quit inside of an interval and the age at blood sample is also in this interval,
    # then we have to adjust the ageAtStop. Since we do not really know when the woman quit, we have
    # to take the middle value between start of interval and age at blood sample.

    ageAtStop <- reestimateStopAge(ageAtStop, ageAtBloodSample)
  }

  if (is.na(ageAtStop)) {
    # If the woman quit smoking at the last interval then there sometimes is no ageAtStop
    # Create an estimate within the last ten interval, using ageAtBloodSample - 1 as
    # first parameter, to make sure that it works also for ages at exacly start of interval.
    # In that case, the selected quit interval will be the one before the ageAtBloodSample

    # But if the start age is missing, then the intervals have not been used.
    if (!is.na(startAge(obs))) {
      ageAtStop <- reestimateStopAge(ageAtBloodSample - 1, ageAtBloodSample)
    } else {
      return(NA)
    }
  }

  tsc <- ageAtBloodSample - ageAtStop

  if (!is.na(tsc) && tsc < 1) {
    # Something is wrong with the data
    tsc <- NA
  }
  
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
