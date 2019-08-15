qSOFAscores <- function(respRate, SBP, mentalScore, septic) {
  
  respPoint <- NULL
  for (i in 1:length(respRate)) {
    if (is.na(respRate[i]) == TRUE) {
      respPoint[i] <- NA
    } else if (respRate[i] >= 22) {
      respPoint[i] <- 1 
    } else if (respRate[i] < 22) {
      respPoint[i] <- 0 
    } else (respPoint[i] <- NA) }
  
  bpPoint <- NULL
  for (i in 1:length(SBP)) {
    if (is.na(SBP[i]) == TRUE) {
      bpPoint[i] <- NA
    } else if (SBP[i] <= 100) {
      bpPoint[i] <- 1 
    } else if (SBP[i] > 100) {
      bpPoint[i] <- 0 
    } else (bpPoint[i] <- NA) }
  
  mentalPoint <- NULL
  for (i in 1:length(mentalScore)) {
    if (is.na(mentalScore[i]) == TRUE) {
      mentalScore[i] <- NA
    } else if (mentalScore[i] < 15) {
      mentalPoint[i] <- 1 
    } else if (mentalScore[i] == 15) {
      mentalPoint[i] <- 0 
    } else (mentalPoint[i] <- NA) }
  
  checking <- as.data.frame(cbind(respPoint, bpPoint, mentalPoint))
  checking$qSOFA <- NA
  
  for (i in 1:nrow(checking)){
    checking$qSOFA[i] <- sum(checking[i,c(1:3)])
  }
  
  return(checking$qSOFA)
  
}