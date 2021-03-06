# Main script for smoking status calculations

rm(list=ls())

load("../data/Controldataset_GEdata_Smokingvars_171018.RData")
# load("/project/data1/tice2/NikitaBaiju/Data/Controldataset_GEdata_Smokingvars_171018.RData")

if (!require(dplyr)) {
  install.packages(dplyr)
  library(dplyr)
}

if (!require(gdata)) {
  install.packages(gdata)
  library(gdata)
}

source("utility_functions.R")
source("find_closest_quest.R")
source("smoking_intensity.R")
source("smoking_status_nfc.R")
source("passive_smoking_status.R")
source("age_at_blood_sample.R")
source("time_since_cessation.R")
source("duration_of_smoking.R")

Qscontrols <- findClosestQuest(Qscontrols)
Qscontrols <- smokingIntensity(Qscontrols)
Qscontrols <- smokingStatusNfc(Qscontrols)
Qscontrols <- setNeverSmokerIntensitiesToZero(Qscontrols)
Qscontrols <- passiveSmokingStatus(Qscontrols)
Qscontrols <- ageAtBloodSample(Qscontrols)
Qscontrols <- timeSinceCessation(Qscontrols)
Qscontrols <- smokingDuration(Qscontrols)



# The following commented out lines can be used for checking the results in Qscontrols.

#table(Qscontrols$SmokingStatus, exclude = NULL)

#currentSmokers <- filter(Qscontrols, SmokingStatus == "Current")
#hist(currentSmokers$SmokingDuration)

#hist(currentSmokers$AgeAtBloodSample)

#formerSmokers <- filter(Qscontrols, SmokingStatus == "Former")
#hist(formerSmokers$SmokingDuration)

# These should all return 0 rows
#filter(Qscontrols, SmokingStatus == "Current" & SmokingDuration == 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Current" & Intensity == 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Current" & TSC != 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Never" & SmokingDuration != 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Never" & Intensity != 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Never" & TSC != 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Former" & SmokingDuration == 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Former" & Intensity == 0) %>% do(data.frame(nrow = nrow(.)))
#filter(Qscontrols, SmokingStatus == "Former" & TSC == 0) %>% do(data.frame(nrow = nrow(.)))
