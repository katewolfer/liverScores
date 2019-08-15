#########################################
## SIRS and sepsis scoring function    ##
## Kate Leary, Institute of Hepatology ##
## 19 June 2018                        ##
#########################################

SIRSscoring <- function(examinationTemp,examinationHR,examinationRespRate,bloodsHaemWBC,LDSseptic) {
  
  # initialise scoring
  score <- 0
  
  # temperature > 38oC: examinationTemp
  if (is.na(examinationTemp) == FALSE) {
    if (examinationTemp > 38 | examinationTemp < 36) {
      score <- score + 1
    }}
  
  # heart rate > 90 beats per min: examinationHR
  if (is.na(examinationHR) == FALSE) {
    if (examinationHR > 90) {
      score <- score + 1
    }}
  
  # respiratory rate > 20 breaths per minute or PaCO2 < 32 mmHg: examinationRespRate
  if (is.na(examinationRespRate) == FALSE) {
    if (examinationRespRate > 20) {
      score <- score + 1
    }}
  
  # leukocytes > 12000 or < 4000mm^3 or 10% pupae: bloodsHaemWBC
  if (is.na(bloodsHaemWBC) == FALSE) {
    if (bloodsHaemWBC > 12) { # 10^9/L is 1000/mm^3
      score <- score + 1
    }}
  
  # sepsis - use calculation for SIRS plus LDSseptic
  sepsis <- 0
  if (is.na(LDSseptic) == FALSE) {
    if (LDSseptic == "Y") { 
      sepsis <- 1
    }}
  
  ## calculate SIRS
  
  # if (score > 1 && sepsis == 0) {
  #   SIRSscore <- "SIRS"
  # } else if (score > 1 && sepsis == 1) {
  #   SIRSscore <- "confirmed sepsis"
  # } else (SIRSscore <- "no SIRS or sepsis")
  
  if (score > 1 && sepsis == 0) {
    SIRSscore <- score
  } else if (score > 1 && sepsis == 1) {
    SIRSscore <- score + sepsis
  } else (SIRSscore <- 0)
  
  return(SIRSscore)
  
}
