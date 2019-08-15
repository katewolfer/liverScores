childPughScore <- function(bilirubin, albumin, ascites, INR, HE) {
  
  ## need to add insertion to check units and convert if wrong
  
  ## tidy up input data
  
  # bilirubin must be umol/L               
  bilirubin <- as.numeric(paste(bilirubin))
  bilirubin[which(is.na(bilirubin)==TRUE)] <- 0 
  
  # serum albumin must be g/dL                    
  albumin <- as.numeric(paste(albumin))
  albumin[which(is.na(albumin)==TRUE)] <- 0 
  
  ascites <- as.numeric(paste(ascites))
  ascites[which(is.na(ascites)==TRUE)] <- 0 
  
  INR <- as.numeric(paste(INR))
  INR[which(is.na(INR)==TRUE)] <- 0 
  
  HE <- as.numeric(paste(HE))
  HE[which(is.na(HE)==TRUE)] <- 0 
  
  # checking <- cbind(bilirubin,subsetMetadata$bloodsBCadmTotalBRSI,albumin,subsetMetadata$bloodsBCadmAlb,
  #                   ascites,subsetMetadata$examinationAscGrade,INR,subsetMetadata$bloodsHaemINR, HE, 
  #                   subsetMetadata$examinationHEgrade)
  
 
  ## get points for each category
  bilirubinPoint <- NULL 
  for (i in 1:length(bilirubin)) {
    if (is.na(bilirubin[i]) == FALSE) {
      if (bilirubin[i] < 34) {
      bilirubinPoint[i] <- 1 
    } else if (bilirubin[i] >= 34 && bilirubin[i] <= 50) {
      bilirubinPoint[i] <- 2 
    } else if (bilirubin[i] > 50) {
      bilirubinPoint[i] <- 3 
    } else (bilirubin[i] <- 0) }}
  
  serAlbuminPoint <- NULL
  for (i in 1:length(albumin)) {
    if (is.na(albumin[i]) == FALSE) {
      if (albumin[i] > 35) {
        serAlbuminPoint[i] <- 1 
      } else if (albumin[i] >= 28 && albumin[i] <= 35) {
        serAlbuminPoint[i] <- 2 
      } else if (albumin[i] < 28 && albumin[i] > 0) {
        serAlbuminPoint[i] <- 3 
      } else (serAlbuminPoint[i] <- 0) }}
  
  INRpoint <- NULL
  for (i in 1:length(INR)) {
    if (is.na(INR[i]) == FALSE) {
      if (INR[i] < 1.7) {
        INRpoint[i] <- 1 
      } else if (INR[i] >= 1.7 && INR[i] <= 2.3) {
        INRpoint[i] <- 2 
      } else if (INR[i] > 2.3) {
        INRpoint[i] <- 3 
      } else (INRpoint[i] <- 0) }}
  
  ascitesPoint <- NULL 
  for (i in 1:length(ascites)) {
    if (is.na(ascites[i]) == FALSE) {
      if (ascites[i] == 0) {
        ascitesPoint[i] <- 1 
      } else if (ascites[i] == 1) {
        ascitesPoint[i] <- 2 
      } else if (ascites[i] >= 2) {
        ascitesPoint[i] <- 3 
      } else (ascites[i] <- 0) }}
  
  HEpoint <- NULL # works
  for (i in 1:length(HE)) {
    if (HE[i] == 0) {
      HEpoint[i] <- 1 
    } else if (HE[i] == 1 || HE[i] == 2) {
      HEpoint[i] <- 2 
    } else if (HE[i] >= 3) {
      HEpoint[i] <- 3 
    } else (HEpoint[i] <- 0) }
  
  # In primary sclerosing cholangitis (PSC) and primary biliary cirrhosis (PBC)
  # the bilirubin references are changed to reflect the fact that these diseases
  # feature high conjugated bilirubin levels
  # The upper limit for 1 point is 68 umol/L and the upper limit for 2 points is 170 umol/L
  
  childPughData <- as.data.frame(cbind(bilirubinPoint,serAlbuminPoint,INRpoint,ascitesPoint,HEpoint)) 
  childPughData$CPscore = apply(childPughData, 1, sum)   # sum each column to get score for each patient
  childPughData$CPscore[which(childPughData$score < 5)] <- "No score"
  #childPughData$score[is.na(childPughData$score) == TRUE] <- "No score"
  
  #finalCP <- cbind(factor(subsetMetadata$baseStudyNumber),childPughData)
  #colnames(finalCP)[1] <- "GLA_studyNumber"
  #write.csv(finalCP, csvTitle)
  
  return(childPughData) }