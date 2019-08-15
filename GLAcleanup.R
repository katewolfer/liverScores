GLAcleanup <- function(metadata, fileName){

#############################################
## clean out unwanted artefacts from Excel ##
#############################################
  
forRemoval <- c("#DIV/0!", "#NAME?", "#NULL!", "#NUM!", "#REF!", "#VALUE!","PENDING")
for (i in 1:ncol(metadata)) {
  for (j in 1:nrow(metadata)) {
    findRemoval <- grep(paste(forRemoval, collapse="|"), paste(metadata[j,i]))
    if (length(findRemoval) > 0) {
      metadata[j,i] <- NA
    }
  }
}

#####################
#####################
## then add scores ##
#####################
#####################

####################################
## Child Pugh numeric calculation ##
####################################

source('childPughScoring.R')
bilirubin <- metadata$bloodsBCadmTotalBRSI # Total bilirubin (umol/L)
albumin <- metadata$bloodsBCadmAlb # Serum albumin (g/dL)
ascites <- metadata$examinationAscGrade
INR <- metadata$bloodsHaemINR
HE <- metadata$examinationHEgrade
childPugh <- childPughScore(bilirubin, albumin, ascites, INR, HE)
childPugh$CPscore <- as.numeric(paste(childPugh$CPscore))
#CP <- cbind(metadata$baseStudyNumber, childPugh)
childPugh$CPscore[which(childPugh$CPscore < 5)] <- NA
metadata$CP <- childPugh$CPscore

###################################
## Child Pugh letter calculation ##
###################################

## CP conversion to classifier
cpClassification <- NULL
for (i in 1:length(childPugh$CPscore)) {
  if (is.na(childPugh$CPscore[i]) == FALSE) {
  if (childPugh$CPscore[i] >= 5 && childPugh$CPscore[i] <= 6) {
    cpClassification[i] <- paste("A",childPugh$CPscore[i], sep="")
  } else if (childPugh$CPscore[i] >= 7 && childPugh$CPscore[i] <= 9) {
    cpClassification[i] <- paste("B",childPugh$CPscore[i], sep="")
  } else if (childPugh$CPscore[i] >= 10 && childPugh$CPscore[i] <= 15) {
    cpClassification[i] <- paste("C",childPugh$CPscore[i], sep="")
  } else {cpClassification[i] <- NA}
  } else {cpClassification[i] <- NA}
}

metadata$CPclass <- cpClassification

############################
## MELD score calculation ##
############################

source("MELDScore.R")
creatinine <- as.numeric(paste(metadata$bloodsBCadmCrCI)) # creatinine (mg/dL)
bilirubin <- as.numeric(paste(metadata$bloodsBCadmTotalBrCI)) # bilirubin (umol/L)
INR <- as.numeric(paste(metadata$bloodsHaemINR)) # INR
dialysis <- as.numeric(paste(metadata$examinationRRT))
Na <- as.numeric(paste(metadata$bloodsBCadmNa))
meld <- MELDScore(creatinine, bilirubin , INR, dialysis, Na, "mg")
metadata$MELD <- round(as.numeric(paste(meld)),0)

############################
## SOFA score calculation ##
############################

source('SOFAscoring.R')
glasgowComa <- metadata$examinationGCStotal # neurological
respiratory <- metadata$bloodsGasPfRatio # respiratory
creatinine <- metadata$bloodsBCadmCrCI # renal
RRT <- metadata$examinationRRT # renal
platelets <- metadata$bloodsHaemPits # coagulation (10^9/L), also 10^3/mm^3, same number
bilirubin <- metadata$bloodsBCadmTotalBrCI# liver
MAP <- metadata$examinationMAPcalc # hypotension
dopamine <- metadata$examinationDobutDose # (ug/kg/min) hypotension
norepinephrine <- metadata$examinationNoradrDose # (ug/kg/min) hypotension
epinephrine <-  metadata$examinationAdrenDose # (ug/kg/min) hypotension
pressors <- metadata$examinationPressors
INR <- metadata$bloodsHaemINR # CLIF-SOFA
HE <- metadata$examinationHEgrade # CLIF-SOFA
terlipressin <- metadata$examinationTerlipressin # Y = 1, N = 0

SOFAscores <- SOFAscoring(respiratory,glasgowComa,MAP,bilirubin,dopamine,creatinine,epinephrine,norepinephrine,platelets,RRT)
metadata$SOFA <- SOFAscores[,7]

#################################
## CLIF SOFA score calculation ##
#################################

source("CLIFSOFAscoring.R")
respiratory <- as.numeric(paste(metadata$bloodsGasPfRatio))
glasgowComa <- as.numeric(paste(metadata$examinationGCStotal))
MAP <- as.numeric(paste(metadata$examinationMAPcalc))
bilirubin <- as.numeric(paste(metadata$bloodsBCadmTotalBrCI))
HE <- as.numeric(paste(metadata$examinationHEgrade))
dopamine <- as.numeric(paste(metadata$examinationDobutDose))
creatinine <- as.numeric(paste(metadata$bloodsBCadmCrCI))
epinephrine <- as.numeric(paste(metadata$examinationAdrenDose))
norepinephrine <- as.numeric(paste(metadata$examinationNoradrDose))
platelets <- as.numeric(paste(metadata$bloodsHaemPits))
RRT <- as.numeric(paste(metadata$examinationRRT))
terlipressin <- as.numeric(paste(metadata$examinationTerlipressin))
INR <- as.numeric(paste(metadata$bloodsHaemINR))

sumPressors <- as.data.frame(cbind(as.numeric(paste(metadata$examinationDobutDose)),as.numeric(paste(metadata$examinationNoradrDose)),
                                   as.numeric(paste(metadata$examinationAdrenDose)),as.numeric(paste(metadata$examinationPressors))))
for (i in 1: nrow(sumPressors)) {
  sumPressors[i,5] <- sum(sumPressors[i,], na.rm = TRUE)
  sumPressors[i,6] <- sumPressors[i,5] > 0
}
pressors <- as.numeric(sumPressors[,6])


CLIFSOFAscores <- CLIFSOFAscoring(respiratory,glasgowComa,MAP,bilirubin,HE,dopamine,creatinine,epinephrine,norepinephrine,terlipressin,platelets,RRT,pressors,INR)

metadata$CLIF_SOFA <- CLIFSOFAscores[,7]

#################################
## CLIF-C-AD score calculation ##
#################################

# CLIF-C ADs <- 10*[0.03*Age{years} + 0.66*Ln(Creatinine{mg/dL}) + 1.71*Ln(INR) + 0.88*Ln(WBC{109cells/L}) -0.05*Sodium{mmol/L} + 8]
age <- as.numeric(paste(metadata$baseAge))
Na <- as.numeric(paste(metadata$bloodsBCadmNa))
creatinine <- as.numeric(paste(metadata$bloodsBCadmCrCI)) # creatinine needs to be mg/dL
WBC <- as.numeric(paste(metadata$bloodsHaemWBC))
INR <- as.numeric(paste(metadata$bloodsHaemINR)) # INR
CLIF_ADscore <- NULL
for (i in 1:length(age)) {
  CLIF_ADscore[i] <- 10 * (0.03*age[i] + 0.66*log(creatinine[i]) + 1.71 * log(INR[i]) + 0.88*log(WBC[i]) - 0.05*Na[i] + 8) }
metadata$CLIF_AD <- round(CLIF_ADscore,0)

###################################
## CLIF-C-ACLF score calculation ##
###################################

## organ failure
liverFail <- as.numeric(paste(metadata$bloodsBCadmTotalBrCI)) # bilirubin (mg/dL) >= 6 and 
kidneyFail <- as.numeric(paste(metadata$bloodsBCadmCrCI))
kidneyFail2 <- as.numeric(paste(metadata$examinationRRT))
brainFail <- as.numeric(paste(metadata$examinationHEgrade))
coagFail <- as.numeric(paste(metadata$bloodsHaemINR))
circFail <- as.numeric(paste(metadata$examinationMAPcalc))
respFail1 <- as.numeric(paste(metadata$examinationSaO2))
respFail2 <- as.numeric(paste(metadata$examinationFiO2)) # respiratory failure: PaO2/FiO2 or SpO2/FiO2, use latter
mechVent <- as.numeric(paste(metadata$examinationSpontVent))
sumPressors <- as.data.frame(cbind(as.numeric(paste(metadata$examinationDobutDose)),as.numeric(paste(metadata$examinationNoradrDose)),
                                   as.numeric(paste(metadata$examinationAdrenDose)),as.numeric(paste(metadata$examinationPressors))))
for (i in 1: nrow(sumPressors)) {
  sumPressors[i,5] <- sum(sumPressors[i,], na.rm = TRUE)
  sumPressors[i,6] <- sumPressors[i,5] > 0
}
pressors <- as.numeric(sumPressors[,6])

## calculate organ failure scores
source("calculateOrganFail.R")
organFail <- calculateOrganFail(liverFail,kidneyFail,kidneyFail2,brainFail,coagFail,circFail,respFail1,respFail2, mechVent, pressors)

# format required columns
age <- as.numeric(paste(metadata$baseAge)) # age: years
WBC <- as.numeric(paste(metadata$bloodsHaemWBC)) # WBC: *10^9 cells/L
CLIFOFs <- organFail$score
metadata$CLIFOFs <- organFail$score ## organ failure
metadata$CLIFOFs[which(metadata$CLIFOFs == 1)] <- NA

## calculate scores
CLIFC_ACLFscore <- NULL
for (i in 1:length(age)) {
  CLIFC_ACLFscore[i] <- 10 * (0.33 * CLIFOFs[i] + 0.04 * age[i] + 0.63 * log(WBC[i]) - 2) }
CLIF_ACLF <- cbind(liverFail,kidneyFail,kidneyFail2,brainFail,coagFail,circFail,respFail1,respFail2, mechVent, pressors, age, WBC, CLIFOFs, CLIFC_ACLFscore)
metadata$CLIF_ACLF <- round(CLIFC_ACLFscore,0)

#############################
## UKELD score calculation ##
#############################

INR <- as.numeric(paste(metadata$bloodsHaemINR)) # INR
creatinine <- as.numeric(paste(metadata$bloodsBCadmCrSI)) # creatinine(µmol/L)
Na <- as.numeric(paste(metadata$bloodsBCadmNa))
bilirubin <- as.numeric(paste(metadata$bloodsBCadmTotalBRSI)) # bilirubin (umol/L)
UKELDscore <- NULL
for (i in 1:length(INR)) {
  UKELDscore[i] <- ((5.395 * log(INR[i])) + (1.485 * log(creatinine[i])) + (3.130 * log(bilirubin[i])) - (81.565 * log(Na[i])) + 435 ) }
UKELD <- cbind(INR, creatinine, bilirubin, Na, UKELDscore)
metadata$UKELD <- round(UKELDscore,0)

#############################
## qSOFA score calculation ##
#############################

source("qSOFAscores.R")
respRate <- as.numeric(paste(metadata$examinationRespRate)) # respiratory rate > 20 breaths per minute or PaCO2 < 32 mmHg
SBP <- as.numeric(paste(metadata$examinationSystolicBP))
mentalScore <- as.numeric(paste(metadata$examinationGCStotal))
septic <- paste(metadata$LDSseptic)  

qSOFA <- qSOFAscores(respRate, SBP, mentalScore, septic)

metadata$qSOFA <- qSOFA


#################################
## Export cleaned data as .csv ##
#################################

write.csv(metadata, paste(fileName,"_CLEAN.csv", sep=""))

## output cleaned metadata for further manipulation
return(metadata)

}