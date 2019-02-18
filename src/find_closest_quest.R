findClosestQuest <- function(women) {
  # Comparing the dates
  women$STARTDAT_X <- as.Date(sprintf("%06d", women$STARTDAT), "%d%m%y")
  women$STARTDAT_Y <- as.Date(sprintf("%06d", women$ystartdat), "%d%m%y")
  women$STARTDAT_Z <- as.Date(sprintf("%06d", women$zstartdat), "%d%m%y")
  women$STARTDAT_B <- as.Date(sprintf("%06d", women$BPROVEDATO), "%d%m%y")
  
  # Find the closest questionnaire
  women$ClosestQuest = "x"
  women$ClosestQuest[!is.na(women$STARTDAT_Y) & women$STARTDAT_B >= women$STARTDAT_Y] <- "y"
  women$ClosestQuest[!is.na(women$STARTDAT_Z) & women$STARTDAT_B >= women$STARTDAT_Z] <- "z"

  return(women)  
}