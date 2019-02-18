# Function for calculating age at blood sample
calculateAgeAtBloodSample <- function (obs) {
  bloodSampleYear <- extractYearFromIntegerDate(convertStringToNumber(obs["BPROVEDATO"]))
  fourDigitBirthYear <- convertStringToNumber(obs["FAAR"]) + 1900
  
  age <- bloodSampleYear - fourDigitBirthYear
  
  return(age)
}

ageAtBloodSample <- function (women) { 
  # To make a new column with the defined function
  women$AgeAtBloodSample <- apply(women, 1, calculateAgeAtBloodSample)
  
  return(women)
}
