# Estimation of never, former, and current smokers

smokingStatusCurrent <- function(woman) {
  currentlySmoking <- FALSE

  closestQuest <- woman["ClosestQuest"]

  broyk <- convertStringToNumber(woman["BROYK"])
  broykYesterday <- convertStringToNumber(woman["BROYKANTGAR"])
  broykToday <- convertStringToNumber(woman["BROYKANTDAG"])

  if (!is.na(broyk) && broyk == 0) {
    currentlySmoking <- TRUE
  } else if (!is.na(broykYesterday) && !is.na(broykToday) && broykYesterday > 0 && broykToday > 0) {
    currentlySmoking <- TRUE
  } else if (!is.na(closestQuest)) {
    smokeNow <- NA

    if (closestQuest == "z") {
      smokeNow <- convertStringToNumber(woman["ZROYKNAA"])
    } else if (closestQuest == "y") {
      smokeNow <- convertStringToNumber(woman["yROYKNAA"])
    } else if (closestQuest == "x") {
      seriesNumber <- convertStringToNumber(woman["SERIENR"])
      if (!is.na(seriesNumber) && seriesNumber <= 9) {
        smokeNow <- convertStringToNumber(woman["ROYKNAAB"])
      } else {
        smokeNow <- convertStringToNumber(woman["ROYKNAA"])
      }
    }

    currentlySmoking <- (!is.na(smokeNow) && smokeNow == 0)
  }

  if (currentlySmoking) {
    return("Current")
  } else {
    return(woman["SmokingStatus"])
  }
}

smokingStatusNfc <- function(women) {
  # Creating a blank column for smoking status
  women$SmokingStatus = NA

  # Current smokers

  women$SmokingStatus <- apply(women, 1, smokingStatusCurrent)
  
  # Never smokers

  women$SmokingStatus[women$ClosestQuest == "z" & is.na(women$SmokingStatus)
                           & (women$ZSIGROYK == 1 | women$ZROYKNAA == 1)
                           & (women$yEVERROK == 1 | is.na(women$yEVERROK))
                           & (women$yEVERROK == 1 | women$yROYKNAA == 1 | women$ySIGROYK == 1)
                           & (women$EVERROK == 1 | is.na(women$EVERROK))
                           & (women$EVERROK == 1 | women$ROYKNAA == 1 | women$ROYKNAAB == 1 | women$SIGROYK == 1)
                           & (is.na(women$Intensity) | women$Intensity <= 0)] <- "Never"

  women$SmokingStatus[women$ClosestQuest == "y" & is.na(women$SmokingStatus)
                           & (women$yEVERROK == 1 | is.na(women$yEVERROK))
                           & (women$yEVERROK == 1 | women$yROYKNAA == 1 | women$ySIGROYK == 1)
                           & (women$EVERROK == 1 | is.na(women$EVERROK))
                           & (women$EVERROK == 1 | women$ROYKNAA == 1 | women$ROYKNAAB == 1 | women$SIGROYK == 1)
                           & (is.na(women$Intensity) | women$Intensity <= 0)] <- "Never"

  women$SmokingStatus[women$ClosestQuest == "x" & is.na(women$SmokingStatus)
                           & (women$EVERROK == 1 | is.na(women$EVERROK))
                           & (women$EVERROK == 1 | women$ROYKNAA == 1 | women$ROYKNAAB == 1 | women$SIGROYK == 1)
                           & (is.na(women$Intensity) | women$Intensity <= 0)] <- "Never"

  # Former smokers

  women$SmokingStatus[women$ClosestQuest == "z" & is.na(women$SmokingStatus)
                           & (women$ZSIGROYK == 0 | women$ZROYKNAA == 0
                              | women$yEVERROK == 0 | women$yROYKNAA == 0 | women$ySIGROYK == 0
                              | women$EVERROK == 0 | women$ROYKNAA == 0 | women$ROYKNAAB == 0 | women$SIGROYK == 0)] <- "Former"

  women$SmokingStatus[women$ClosestQuest == "y" & is.na(women$SmokingStatus)
                           & (women$yEVERROK == 0 | women$yROYKNAA == 0 | women$ySIGROYK == 0
                              | women$EVERROK == 0 | women$ROYKNAA == 0 | women$ROYKNAAB == 0 | women$SIGROYK == 0)] <- "Former"

  women$SmokingStatus[women$ClosestQuest == "x" & is.na(women$SmokingStatus)
                           & (women$EVERROK == 0 | women$ROYKNAA == 0 | women$ROYKNAAB == 0 | women$SIGROYK == 0)] <- "Former"

  # Women who still have no smoking status but have intensities are former smokers
  women$SmokingStatus[is.na(women$SmokingStatus) & women$Intensity > 0] <- "Former"

  # Women who still have no smoking status but have answered that they have never smoked
  women$SmokingStatus[is.na(women$SmokingStatus) & (women$EVERROK == 1 | women$yEVERROK == 1)] <- "Never"

  return(women)
}
