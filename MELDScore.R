MELDScore <- function(creatinine, bilirubin, INR, dialysis, Na, units) {
  
  #####################################
  ## Kate Leary                      ##
  ## Institute of Hepatology, London ##
  ## 03 October 2018                 ##
  #####################################
  
  ## INPUTS:
  ## All inputs need to be vectors of matching length
  ## Creatinine units: mg/dL
  ## Bilirubin units: mg/dL
  ## INR: international normalised ratio, dimensionless
  ## Dialysis: 0 = has not recieved, 1 has recieved 
  ## Units: "mg" for mg/dL (preferred but is converted if not), "umol" for umol/L
  
  ## to convert SI units for creatinine and bilirubin into the ones used for the score calculation
  if (units == "mg") {
  } else if (units == "umol") {
    creatinine <- creatinine/10 # get it into dL
    creatinine <- creatinine/1000000 # convert into mol not umol
    creatinine <- creatinine*113.12 # convert to g
    creatinine <- creatinine*1000 # convert to mg
    
    bilirubin <- bilirubin/10 # get it into dL
    bilirubin <- bilirubin/1000000 # convert into mol not umol
    bilirubin <- bilirubin*584.67 # convert to g
    bilirubin <- bilirubin*1000 # convert to mg
  }
  
  ###########################################################################
  ## From the UNOS Policy notice 11/2015: OPTN Executive Committee Actions ##
  ## Guidelines for score calculation                                      ##
  ###########################################################################
  
  ## Candidates who are at least 12 years old receive an initial MELD(i) score equal to: 
  ## 0.957 x Loge(creatinine mg/dL) + 0.378 x Loge(bilirubin mg/dL) + 1.120 x Loge (INR) + 0.643 
  
  ## Laboratory values less than 1.0 will be set to 1.0 when calculating a candidate's MELD score
  #creatinine[which(is.na(creatinine == TRUE))] <- 1
  creatinine[which(creatinine < 1)] <- 1
  #bilirubin[which(is.na(bilirubin == TRUE))] <- 1
  bilirubin[which(bilirubin < 1)] <- 1
  INR[which(INR < 1)] <- 1
  #INR[which(is.na(INR == TRUE))] <- 1
  
  ## The following candidates will receive a creatinine value of 4.0 mg/dL:
  ## Candidates with a creatinine value greater than 4.0 mg/dL ???
  ## Candidates who received two or more dialysis treatments within the prior 7 days ???
  ## Candidates who received 24 hours of continuous veno-venous hemodialysis (CVVHD) within the prior 7 days
  creatinine[which(dialysis >= 1)] <- 4
  creatinine[which(creatinine > 4)] <- 4
  dialysis[which(is.na(dialysis)==TRUE)] <- 0
  dialysis[which(dialysis > 1)] <- 1
  
  ## The maximum MELD score is 40. 
  ## The MELD score derived from this calculation will be rounded to the tenth decimal place and then multiplied by 10.
  ## For candidates with an initial MELD score greater than 11, the MELD score is then re-calculated as follows:
  ## MELD = MELD(i) + 1.32*(137-Na) - [0.033*MELD(i)*(137-Na)]
  ## Sodium values less than 125 mmol/L will be set to 125, and values greater than 137 mmol/L will be set to 137
  Na[which(Na < 125)] <- 125
  Na[which(Na > 137)] <- 137
  
  
  ######################
  ## Calculate scores ##
  ######################
  
  meldScore <- NULL
  
  # Score calculation
  # 0.957 x Loge(creatinine mg/dL) + 0.378 x Loge(bilirubin mg/dL) + 1.120 x Loge (INR) + 0.643 
  for (k in 1:length(creatinine)) {
    meldScore[k] <- 10 * (((0.957*log(creatinine[k])) + (0.378*log(bilirubin[k])) + (1.12*log(INR[k]))) + 0.643)
  }
  
  # For scores falling outside defined boundaries
  for (i in 1:length(meldScore)) {
    if (is.na(meldScore[i] == TRUE)) {
    } else {
      if (meldScore[i] < 6) {
        meldScore[i] <- 6
      } else if (meldScore[i] > 40) {
        meldScore[i] <- 40
      }
    }
  }
  
  ## Deal with MELD scores > 11
  for (j in 1:length(meldScore)) {
    if (is.na(meldScore[j] == FALSE)) {
    } else if (meldScore[j] > 11) {
      # MELD = MELD(i) + 1.32*(137-Na) - [0.033*MELD(i)*(137-Na)]
      meldScore[j] <- meldScore[j] + 1.32*(137-Na[j]) - (0.033*meldScore[j] * (137-Na[j]))
    } 
  }
  
  ## Round number output
  meldScore <- round(meldScore, 2)
  
  ## Tidy up scores which have NAs to make it clear no score obtained
  #meldScore[is.na(meldScore) == TRUE] <- "No score"
  
  ## Output scores
  return(meldScore)
  
}


