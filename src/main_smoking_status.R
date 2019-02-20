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

