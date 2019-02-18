### Passive smoking status

passiveSmokingStatus <- function (women) {
  # Creating a blank column for passive smoking status
  women$PassiveSmoking = NA
  
  # Passive_childhood
  
  women$PassiveSmoking[women$ClosestQuest == "z" 
                            & (women$ZROKBARN ==0 |women$ZBARNROYK == 0 
                               | women$yROKBARN == 0 
                               | women$ROKBARN ==0)] <- "Passive_childhood"
  
  women$PassiveSmoking[women$ClosestQuest == "y" 
                            & (women$yROKBARN == 0 
                               | women$ROKBARN ==0)] <- "Passive_childhood"
  
  women$PassiveSmoking[women$ClosestQuest == "x" 
                            & women$ROKBARN ==0] <- "Passive_childhood" 
  
  # Passive_adulthood
  
  women$PassiveSmoking[is.na(women$PassiveSmoking)  
                            & (women$ClosestQuest == "z" | women$ClosestQuest == "y")
                            & (women$yROKBOR == 0 | women$yROKBOR == 3 | women$yROKBOR == 4 | women$yROYKARB > 0
                               | women$ROKBOR ==0)] <- "Passive_adulthood"
  
  women$PassiveSmoking[is.na(women$PassiveSmoking)  
                            & women$ClosestQuest == "x" 
                            & women$ROKBOR ==0] <- "Passive_adulthood"
  
  # Passive_both (here it overwrites passive childhood)
  
  women$PassiveSmoking[women$PassiveSmoking == "Passive_childhood"
                            & (women$ClosestQuest == "z" | women$ClosestQuest == "y")
                            & (women$yROKBOR == 0 | women$yROKBOR == 3 | women$yROKBOR == 4 | women$yROYKARB > 0
                               | women$ROKBOR == 0)] <- "Passive_both" 
  
  women$PassiveSmoking[women$PassiveSmoking == "Passive_childhood"
                            & women$ClosestQuest == "x" 
                            & women$ROKBOR ==0] <- "Passive_both" 
  
  # Not_PassiveSmoker
  
  women$PassiveSmoking[women$ClosestQuest == "z" & is.na(women$PassiveSmoking)
                            & (women$ZROKBARN ==1 |women$ZBARNROYK == 1 
                               | women$yROKBARN == 1 
                               | women$ROKBARN ==1)
                            & (women$yROKBOR == 1 | women$ROKBOR ==1)
                            & (women$yROYKARB == 0 | is.na(women$yROYKARB))] <- "Not_PassiveSmoker"
  
  
  women$PassiveSmoking[women$ClosestQuest == "y" & is.na(women$PassiveSmoking)
                            & (women$yROKBARN == 1 
                               | women$ROKBARN ==1)
                            & (women$yROKBOR == 1 | women$ROKBOR ==1)
                            & (women$yROYKARB == 0 | is.na(women$yROYKARB))] <- "Not_PassiveSmoker"
  
  women$PassiveSmoking[women$ClosestQuest == "x" & is.na(women$PassiveSmoking)
                            & women$ROKBARN ==1
                            &  women$ROKBOR ==1] <- "Not_PassiveSmoker"
  
  
  addmargins(table(women$PassiveSmoking, exclude=NULL))
  
  Adult_passivesmoking <- filter(.data=women, women$PassiveSmoking == "Passive_adulthood")
  
  Childhood_passivesmoking <- filter(.data=women, women$PassiveSmoking == "Passive_childhood")
  
  Missing_passivesmoking <- filter(.data=women, is.na(PassiveSmoking))
  # Remember to include some information about 18 missing smoking status in the report/article
  
  # Remember to include some information about 172 missing passive values in the report/article
  addmargins(table(Missing_passivesmoking$ClosestQuest))
  addmargins(table(Missing_passivesmoking$SERIENR))
  
  addmargins(table(women$SERIENR))
  
  return(women)
}