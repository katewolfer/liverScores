#########################################
## CLIF-SOFA scoring                   ##
## Kate Leary, Institute of Hepatology ##
## 25 June 2018                        ##
#########################################

CLIFSOFAscoring <- function(respiratory,glasgowComa,MAP,bilirubin,HE,dopamine,creatinine,epinephrine,norepinephrine,terlipressin,platelets,RRT,pressors,INR) {
  
  ## ensure columns are correctly formatted
  respiratory <- as.numeric(paste(respiratory))
  #respiratory[which(is.na(respiratory)==TRUE)] <- 0 
  
  MAP <- as.numeric(paste(MAP))
  MAP[which(is.na(MAP)==TRUE)] <- 0 
  
  HE <- as.numeric(paste(HE))
  HE[which(is.na(HE)==TRUE)] <- 0 
  
  bilirubin <- as.numeric(paste(bilirubin))
  #bilirubin[which(is.na(bilirubin)==TRUE)] <- 0 
  
  creatinine <- as.numeric(paste(creatinine))
  #creatinine[which(is.na(creatinine)==TRUE)] <- 0 
  
  dopamine <- as.numeric(paste(dopamine))
  dopamine[which(is.na(dopamine)==TRUE)] <- 0 
  
  epinephrine <- as.numeric(paste(epinephrine))
  epinephrine[which(is.na(epinephrine)==TRUE)] <- 0 
  
  norepinephrine <- as.numeric(paste(norepinephrine))
  norepinephrine[which(is.na(norepinephrine)==TRUE)] <- 0 
  
  terlipressin <- as.numeric(paste(terlipressin))
  terlipressin[which(is.na(terlipressin)==TRUE)] <- 0 
  
  INR <- as.numeric(paste(INR))
  #INR[which(is.na(INR)==TRUE)] <- 0 
  
  platelets <- as.numeric(paste(platelets))
  #platelets[which(is.na(platelets)==TRUE)] <- 0
  
  RRT <- as.numeric(paste(RRT))
  #RRT[which(is.na(RRT)==TRUE)] <- 0
  
  pressors <- as.numeric(paste(pressors))
  #pressors[which(is.na(pressors)==TRUE)] <- 0
  
  # respiration points, SaO2/FiO2
  #respPoint <- NULL
  #respScore <- NULL
  #for (i in 1:length(respiratory1)) {
  #  respScore[i] <- respiratory1[i]/respiratory2[i]
  #  if (respScore[i] == 0 || is.na(respScore[i]) == TRUE) {
  #    respPoint[i] <- 0 
  #  } else if (respScore[i] > 301) {
  #    respPoint[i] <- 0 
  #  } else if (respScore[i] >= 221 && respScore[i] <= 301) {
  #    respPoint[i] <- 1 
  #  } else if (respScore[i] >= 142 && respScore[i] < 221) {
  #    respPoint[i] <- 2 
  #  } else if (respScore[i] >= 67 && respScore[i] < 142) {
  #    respPoint[i] <- 3
  #  } else if (respScore[i] < 67) {
  #    respPoint[i] <- 4
  #  } else (stop("Missing respiratory score value(s)")) }
  
  # respiration points,PaO2/FiO2
  respPoint <- NULL
  for (i in 1:length(respiratory)) {
    if (is.na(respiratory[i]) == TRUE) {
      respPoint[i] <- NA
    } else if (respiratory[i] == 0) {
      respPoint[i] <- 0 
    } else if (respiratory[i] > 400) {
      respPoint[i] <- 0 
    } else if (respiratory[i] > 300 && respiratory[i] <= 400) {
      respPoint[i] <- 1 
    } else if (respiratory[i] > 200 && respiratory[i] <= 300) {
      respPoint[i] <- 2 
    } else if (respiratory[i] > 100 && respiratory[i] <= 200) {
      respPoint[i] <- 3
    } else if (respiratory[i] <= 100) {
      respPoint[i] <- 4
    } else (respPoint[i] <- NA) }
  
  # coagulation points
  coagPoint <- NULL
  for (i in 1:length(INR)) {
    if (is.na(INR[i]) == TRUE) {
      coagPoint[i] <- NA 
    } else if (INR[i] < 1.1) {
      coagPoint[i] <- 0 
    } else if (INR[i] >= 1.1 && INR[i] < 1.25) {
      coagPoint[i] <- 1 
    } else if (INR[i] >= 1.25 && INR[i] < 1.5) {
      coagPoint[i] <- 2
    } else if (INR[i] >= 1.5 && INR[i] < 2.5) {
      coagPoint[i] <- 3
    } else if (INR[i] >= 2.5 || platelets[i] <= 20) {
      coagPoint[i] <- 4
    } else (coagPoint[i] <- NA) }
  
  # liver - bilirubin points
  bilirubinPoint <- NULL
  for (i in 1:length(bilirubin)) {
    if (is.na(bilirubin[i]) == TRUE) {
      bilirubinPoint[i] <- NA
    } else if (bilirubin[i] < 1.2) {
      bilirubinPoint[i] <- 0
    } else if (bilirubin[i] >= 1.2 && bilirubin[i] < 2.0) {
      bilirubinPoint[i] <- 1
    } else if (bilirubin[i] >= 2.0 && bilirubin[i] < 6.0) {
      bilirubinPoint[i] <- 2
    } else if (bilirubin[i] >= 6.0 && bilirubin[i] < 12.0) {
      bilirubinPoint[i] <- 3
    } else if (bilirubin[i] >= 12.0) {
      bilirubinPoint[i] <- 4
    } else (bilirubinPoint[i] <- NA) }
  
  # bilirubinPoint <- NULL 
  # for (i in 1:length(bilirubin)) {
  #   if (is.na(bilirubin[i]) == TRUE) {
  #     bilirubinPoint[i] <- NA 
  #   } else if (bilirubin[i] < 20) {
  #     bilirubinPoint[i] <- 0 
  #   } else if (bilirubin[i] >= 20 && bilirubin[i] < 33) {
  #     bilirubinPoint[i] <- 1 
  #   } else if (bilirubin[i] >= 33 && bilirubin[i] < 102) {
  #     bilirubinPoint[i] <- 2
  #   } else if (bilirubin[i] >= 102 && bilirubin[i] < 204) {
  #     bilirubinPoint[i] <- 3
  #   } else if (bilirubin[i] >= 204) {
  #     bilirubinPoint[i] <- 4
  #   } else (bilirubinPoint[i] <- NA) }
  
  # # hypotension points THIS PART HAS THE ISSUE
  # hypotensionPoint <- NULL
  # for (i in 1:length(MAP)) {
  #   if (is.na(MAP[i]) == TRUE || is.na(pressors[i]) == TRUE || is.na(dopamine[i]) == TRUE || is.na(epinephrine[i]) == TRUE || is.na(norepinephrine[i]) == TRUE ) {
  #     hypotensionPoint[i] <- NA } 
  #   else if (MAP[i] > 70 && pressors[i] == 0) {
  #     hypotensionPoint[i] <- 0
  #   } else if (MAP[i] <= 70 && pressors[i] == 0) {
  #     hypotensionPoint[i] <- 1
  #   } else if (dopamine[i] > 0) {
  #     hypotensionPoint[i] <- 2
  #   } else if (norepinephrine[i] > 0 && norepinephrine[i] <= 0.1 && pressors[i] == 1 || epinephrine[i] > 0 && epinephrine[i] <= 0.1 && pressors[i] == 1) {
  #     hypotensionPoint[i] <- 3
  #   } else if (norepinephrine[i] > 0.1 || epinephrine[i] > 0.1) {
  #     hypotensionPoint[i] <- 4
  #   } else (hypotensionPoint[i] <- NA) }
  
  # cardiovascular points
  checking <- as.data.frame(cbind(MAP,dopamine,norepinephrine,epinephrine,terlipressin))
  checking["hypotensionPoint"] <- NA
  
  for (i in 1:nrow(checking)) {
    if (sum(checking[i,1:4]) == 0) {
      checking$hypotensionPoint[i] <- NaN
    } else if (checking$epinephrine[i] > 0.1 || checking$norepinephrine[i] > 0.1) {
      checking$hypotensionPoint[i] <- 4
    } else if (checking$dopamine[i] > 0 || checking$terlipressin[i] == 1) {
      checking$hypotensionPoint[i] <- 2
    } else if (checking$epinephrine[i] <= 0.1 && checking$epinephrine[i] > 0) {
      checking$hypotensionPoint[i] <- 3
    } else if (checking$norepinephrine[i] <= 0.1 && checking$norepinephrine[i] > 0) {
      checking$hypotensionPoint[i] <- 3
    } else if (checking$MAP[i] < 70 && checking$MAP[i] > 0) {
      checking$hypotensionPoint[i] <- 1
    }  else if (checking$MAP[i] >= 70) {
      checking$hypotensionPoint[i] <- 0
    } 
  }
  
  hypotensionPoint <- checking$hypotensionPoint
  
  # neurological points
  HEpoint <- NULL
  for (i in 1:length(HE)) {
    if (is.na(HE[i]) == TRUE || length(HE[i]) < 1) {
      HEpoint[i] <- NA
      } else if (HE[i] == 0) {
        HEpoint[i] <- 1 
      } else if (HE[i] == 1 || HE[i] == 2) {
        HEpoint[i] <- 2 
      } else if (HE[i] == 3) {
        HEpoint[i] <- 3 
      } else if (HE[i] >= 4) {
        HEpoint[i] <- 4
      } else (HEpoint[i] <- NA) }
  
  # creatinine points - brackets are different on spreadsheet
  creatininePoint <- NULL
  for (i in 1:length(creatinine)) {
    if (is.na(creatinine[i]) == TRUE) {
      creatininePoint[i] <- NA
    } else if (creatinine[i] < 1.2) {
      creatininePoint[i] <- 0 
    } else if (creatinine[i] >= 1.2 && creatinine[i] < 2.0) {
      creatininePoint[i] <- 1
    } else if (creatinine[i] >= 2.0 && creatinine[i] < 3.5) {
      creatininePoint[i] <- 2
    } else if (creatinine[i] >= 3.5 && creatinine[i] <= 4.9) {
      creatininePoint[i] <- 3
    } else if (creatinine[i] >= 5.0) {
      creatininePoint[i] <- 4 
    } else if (RRT[i] == 1) {
      creatininePoint[i] <- 3
    } else (creatininePoint[i] <- NA) }
  
  # sum all points to obtain scores
  CLIFSOFAscores <- as.data.frame(cbind(respPoint,coagPoint,bilirubinPoint,hypotensionPoint,HEpoint,creatininePoint))
  CLIFSOFAscores$score <- apply(CLIFSOFAscores, 1, sum,na.rm=TRUE) # sum each column to get score for each patient
  #CLIFSOFAscores$score[is.na(SOFAscores$score) == TRUE] <- "No score"
  
  return(CLIFSOFAscores)
  
}