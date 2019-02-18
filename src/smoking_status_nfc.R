# Current smokers from the blood sample questionnaires

smokingStatusNfc <- function (women) {
  # Creating a blank column for smoking status
  women$SmokingStatus = NA 
  
  # Current smokers
  
  women$SmokingStatus[women$BROYK == 0] <- "Current"
  women$SmokingStatus[women$BROYKANTGAR > 0 & women$BROYKANTDAG > 0] <- "Current"
  
  # Never smokers
  
  women$SmokingStatus[women$ClosestQuest == "z" & is.na(women$SmokingStatus)
                           & (women$ZSIGROYK == 1 |women$ZROYKNAA ==1) 
                           & (women$yEVERROK == 1 | is.na(women$yEVERROK))
                           & (women$yEVERROK==1 | women$yROYKNAA ==1 | women$ySIGROYK == 1 ) 
                           & (women$EVERROK == 1 | is.na(women$EVERROK))
                           & (women$EVERROK==1 | women$ROYKNAA ==1 | women$ROYKNAAB ==1 | women$SIGROYK ==1)
                           & (is.na(women$Intensity) | women$Intensity <= 0)] <- "Never" 
  
  women$SmokingStatus[women$ClosestQuest == "y" & is.na(women$SmokingStatus)
                           & (women$yEVERROK == 1 | is.na(women$yEVERROK))
                           & (women$yEVERROK==1 | women$yROYKNAA ==1 |women$ySIGROYK ==1 ) 
                           & (women$EVERROK == 1 | is.na(women$EVERROK))
                           & (women$EVERROK==1 | women$ROYKNAA ==1 | women$ROYKNAAB ==1 | women$SIGROYK ==1)
                           & (is.na(women$Intensity) | women$Intensity <= 0)] <- "Never" 
  
  women$SmokingStatus[women$ClosestQuest == "x" & is.na(women$SmokingStatus)
                           & (women$EVERROK == 1 | is.na(women$EVERROK))
                           & (women$EVERROK==1 | women$ROYKNAA ==1 | women$ROYKNAAB ==1 | women$SIGROYK ==1)
                           & (is.na(women$Intensity) | women$Intensity <= 0)] <- "Never" 
  
  # Former smokers
  
  women$SmokingStatus[women$ClosestQuest == "z" & is.na(women$SmokingStatus)
                           & (women$ZSIGROYK ==0 |women$ZROYKNAA ==0 
                              | women$yEVERROK ==0 |women$yROYKNAA ==0 |women$ySIGROYK ==0 
                              | women$EVERROK==0  | women$ROYKNAA ==0 | women$ROYKNAAB ==0 | women$SIGROYK ==0)] <- "Former"  
  
  women$SmokingStatus[women$ClosestQuest == "y" & is.na(women$SmokingStatus)
                           & (women$yEVERROK ==0 |women$yROYKNAA ==0 |women$ySIGROYK ==0 
                              | women$EVERROK==0  | women$ROYKNAA ==0 | women$ROYKNAAB ==0 | women$SIGROYK ==0)] <- "Former" 
  
  women$SmokingStatus[women$ClosestQuest == "x" & is.na(women$SmokingStatus)
                           & (women$EVERROK==0  | women$ROYKNAA ==0 | women$ROYKNAAB ==0 | women$SIGROYK ==0)] <- "Former"
  
  
  return(women)
}