SOFAscoring <- function(respiratory,glasgowComa,MAP,bilirubin,dopamine,creatinine,epinephrine,norepinephrine,platelets,RRT, units) {
  
  ############################################################
  ## SOFA scoring                                           ##
  ## Kate Leary, Institute of Hepatology                    ##
  ## 21 June 2018                                           ##
  ############################################################
  
  ## source: https://journals.lww.com/ccmjournal/Fulltext/1998/11000/Use_of_the_SOFA_score_to_assess_the_incidence_of.16.aspx
  
  ## ensure columns are correctly formatted
  respiratory <- as.numeric(paste(respiratory))
  #respiratory[which(is.na(respiratory)==TRUE)] <- 0 
  
  glasgowComa <- as.numeric(paste(glasgowComa))
  #glasgowComa[which(is.na(glasgowComa)==TRUE)] <- 0 
  
  MAP <- as.numeric(paste(MAP))
  MAP[which(is.na(MAP)==TRUE)] <- 0 
  
  bilirubin <- as.numeric(paste(bilirubin))
  #bilirubin[which(is.na(bilirubin)==TRUE)] <- 0 
  
  creatinine <- as.numeric(paste(creatinine))
  #creatinine[which(is.na(creatinine)==TRUE)] <- 0 
  
  RRT <- as.numeric(paste(RRT))
  #RRT[which(is.na(RRT)==TRUE)] <- 0
  
  dopamine <- as.numeric(paste(dopamine))
  dopamine[which(is.na(dopamine)==TRUE)] <- 0 
  
  epinephrine <- as.numeric(paste(epinephrine))
  epinephrine[which(is.na(epinephrine)==TRUE)] <- 0 
  
  norepinephrine <- as.numeric(paste(norepinephrine))
  norepinephrine[which(is.na(norepinephrine)==TRUE)] <- 0 
  
  platelets <- as.numeric(paste(platelets))
  #platelets[which(is.na(platelets)==TRUE)] <- 0
  
  scoringCheck <- as.data.frame(cbind(respiratory,glasgowComa,MAP,bilirubin,creatinine,RRT,dopamine,epinephrine,norepinephrine,platelets))
  #pressors <- as.numeric(paste(pressors))
  #pressors[which(is.na(pressors)==TRUE)] <- 0
  
  #vasopressin <- as.numeric(paste(vasopressin))
  #vasopressorOther <- as.numeric(paste(vasopressorOther))

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
      respPoint[i] <- NA 
    } else if (respiratory[i] > 400) {
      respPoint[i] <- 0 
    } else if (respiratory[i] > 300 && respiratory[i] <= 400) {
      respPoint[i] <- 1 
    } else if (respiratory[i] > 200 && respiratory[i] <= 300) {
      respPoint[i] <- 2 
    } else if (respiratory[i] > 100 && respiratory[i] <= 200) {
      respPoint[i] <- 3
    } else if (respiratory[i] <= 100 && respiratory[i] > 0) {
      respPoint[i] <- 4
    } else (respPoint[i] <- NA) }
  
  # coagulation points
  coagPoint <- NULL
  for (i in 1:length(platelets)) {
    if (is.na(platelets[i]) == TRUE) {
      coagPoint[i] <- NA 
    } else if (platelets[i] == 0) {
        coagPoint[i] <- NA 
    } else if (platelets[i] > 150) {
      coagPoint[i] <- 0 
    } else if (platelets[i] >= 100 && platelets[i] <= 150) {
      coagPoint[i] <- 1 
    } else if (platelets[i] >= 50 && platelets[i] < 100) {
      coagPoint[i] <- 2
    } else if (platelets[i] >= 20 && platelets[i] < 50) {
      coagPoint[i] <- 3
    } else if (platelets[i] < 20 && platelets[i] > 0) {
      coagPoint[i] <- 4
    } else (coagPoint[i] <- NA) }
  
  # liver - bilirubin points mg/dL
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
  #     bilirubinPoint[i] <- NA } 
  #   else if (bilirubin[i] < 20) {
  #     bilirubinPoint[i] <- 0 
  #   } else if (bilirubin[i] >= 20 && bilirubin[i] < 33) {
  #     bilirubinPoint[i] <- 1 
  #   } else if (bilirubin[i] >= 33 && bilirubin[i] < 102) {
  #     bilirubinPoint[i] <- 2
  #   } else if (bilirubin[i] >= 102 && bilirubin[i] < 204) {
  #     bilirubinPoint[i] <- 3
  #   } else if (bilirubin[i] >= 204) {
  #     bilirubinPoint[i] <- 4
  #   } else (bilirubinPoint[i] <- NA ) }
  
  
  # cardiovascular points
  checking <- as.data.frame(cbind(MAP,dopamine,norepinephrine,epinephrine))
  checking["hypotensionPoint"] <- NA
  
  for (i in 1:nrow(checking)) {
    if (sum(checking[i,1:4]) == 0) {
      checking$hypotensionPoint[i] <- NaN
    } else if (checking$epinephrine[i] > 0.1 || checking$norepinephrine[i] > 0.1) {
      checking$hypotensionPoint[i] <- 4
    } else if (checking$dopamine[i] > 0) {
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

  
  # Glasgow coma points - this differs on the spreadsheet
  # https://onlinelibrary.wiley.com/doi/full/10.1034/j.1600-0676.2002.00001.x
  comaPoint <- NULL
  for (i in 1:length(glasgowComa)) {
    if (is.na(glasgowComa[i]) == TRUE) {
      comaPoint[i] <- NA
    } else if (glasgowComa[i] >= 15) {
      comaPoint[i] <- 0 
    } else if (glasgowComa[i] < 6) {
      comaPoint[i] <- 0 
    } else if (glasgowComa[i] == 13 || glasgowComa[i] == 14) {
      comaPoint[i] <- 1
    } else if (glasgowComa[i] >= 10 && glasgowComa[i] <= 12) {
      comaPoint[i] <- 2
    } else if (glasgowComa[i] >= 6 && glasgowComa[i] <= 9) {
      comaPoint[i] <- 3
    } else if (glasgowComa[i] < 6 && glasgowComa[i] > 0) {
      comaPoint[i] <- 4 
    } else (comaPoint[i] <- NA) }
  
  # creatinine points - brackets are different on spreadsheet
  creatininePoint <- NULL
  for (i in 1:length(creatinine)) {
   if (is.na(RRT[i]) == FALSE && RRT[i] == 1) {
    creatininePoint[i] <- 3 }
    else if (is.na(creatinine[i]) == TRUE) {
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
    } else (creatininePoint[i] <- NA) }
  
  # sum all points to obtain scores
  SOFAscores <- as.data.frame(cbind(respPoint,coagPoint,bilirubinPoint,hypotensionPoint,comaPoint,creatininePoint))
  SOFAscores$score <- apply(SOFAscores, 1, sum,na.rm=TRUE) # sum each column to get score for each patient
  SOFAscores$score[is.na(SOFAscores$score) == TRUE] <- "No score"
  
  return(SOFAscores)
  
}