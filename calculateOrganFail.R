############################################################
## Calculating organ failure scoring for CLIF scores      ##
## Kate Leary, Institute of Hepatology                    ##
## 19 June 2018                                           ##
############################################################

# this function calculates the organ failure score for each patient in the list
# respiratory failure: PaO2/FiO2 or SpO2/FiO2, use latter for GLA

calculateOrganFail <- function(liverFail,kidneyFail,kidneyFail2,brainFail,coagFail,circFail,respFail1,respFail2, mechVent, pressors) {

## ensure columns are correctly formatted
liverFail <- as.numeric(paste(liverFail))
kidneyFail <- as.numeric(paste(kidneyFail))
kidneyFail2 <- as.numeric(paste(kidneyFail2))
brainFail <- as.numeric(paste(brainFail))
coagFail <- as.numeric(paste(coagFail))
circFail <- as.numeric(paste(circFail))
respFail1 <- as.numeric(paste(respFail1))
respFail2 <- as.numeric(paste(respFail2))
mechVent <- as.numeric(paste(mechVent))
pressors <- as.numeric(paste(pressors))

# ## deal with NAs
# liverFail[which(is.na(liverFail)==TRUE)] <- 0
# kidneyFail[which(is.na(kidneyFail)==TRUE)] <- 0
# kidneyFail2[which(is.na(kidneyFail2)==TRUE)] <- 0
# brainFail[which(is.na(brainFail)==TRUE)] <- 0
# coagFail[which(is.na(coagFail)==TRUE)] <- 0
# circFail [which(is.na(circFail )==TRUE)] <- 0
# respFail1[which(is.na(respFail1)==TRUE)] <- 0
# respFail2[which(is.na(respFail2)==TRUE)] <- 0
# mechVent[which(is.na(mechVent)==TRUE)] <- 0
# pressors[which(is.na(pressors)==TRUE)] <- 0

## calculate liver points
# liverPoint <- NULL
# for (i in 1:length(liverFail)) {
#   if (liverFail[i] == 0) {
#     liverPoint[i] <- 0
#   } else if (liverFail[i] < 6) {
#     liverPoint[i] <- 1
#   } else if (liverFail[i] >= 6 && liverFail[i] <= 12) {
#     liverPoint[i] <- 2
#   } else if (liverFail[i] > 12) {
#     liverPoint[i] <- 3
#   #} else (stop("Missing liver score value")) }
#   } else (liverPoint[i] <- 0) }

liverPoint <- liverFail
liverPoint[which(liverPoint < 6)] <- 0
liverPoint[which(liverFail >= 6 & liverFail <= 12)] <- 2
liverPoint[which(liverFail > 12)] <- 3


## calculate kidney points
kidneyPoint <- NULL
for (i in 1:length(kidneyFail)) {
  if (kidneyFail[i] == 0) {
    kidneyPoint[i] <- 0
  } else if (kidneyFail[i] < 2) {
    kidneyPoint[i] <- 1
  } else if (kidneyFail[i] >= 2 && kidneyFail[i] <= 3.5) {
    kidneyPoint[i] <- 2
  } else if (kidneyFail[i] > 3.5 || kidneyFail2[i] == 1) {
    kidneyPoint[i] <- 3
  #} else (stop("Missing kidney score value")) }
  } else (kidneyPoint[i] <- 0) }

kidneyPoint <- kidneyFail
kidneyPoint[which(kidneyPoint == 0)] <- 0
kidneyPoint[which(kidneyPoint > 0 & kidneyFail < 2)] <- 1
kidneyPoint[which(kidneyFail >= 2 & kidneyFail <= 3.5)] <- 2
kidneyPoint[which(kidneyFail > 12)] <- 3

## calculate brain points
brainPoint <- NULL
for (i in 1:length(brainFail)) {
  if (brainFail[i] == 0) {
    brainPoint[i] <- 1
  } else if (brainFail[i] == 1 || brainFail[i] == 2) {
    brainPoint[i] <- 2
  #} else if (brainFail[i] == 3 || brainFail[i] == 4)  {
  } else if (brainFail[i] >= 3)  {
    brainPoint[i] <- 3
  #} else (stop("Missing brain score value")) }
  } else (brainPoint[i] <- 0) }

## calculate coagulation points
coagPoint <- NULL
for (i in 1:length(coagFail)) {
  if (coagFail[i] == 0) {
    coagPoint[i] <- 0
  } else if (coagFail[i] < 2) {
    coagPoint[i] <- 1
  } else if (coagFail[i] >= 2 && coagFail[i] <= 2.5) {
    coagPoint[i] <- 2
  } else if (coagFail[i] > 2.5) {
    coagPoint[i] <- 3
  #} else (stop("Missing coagulation score value")) }
  } else (coagPoint[i] <- 0) }

## calculate circulation points
circPoint <- NULL
for (i in 1:length(circFail)) {
  if (circFail[i] == 0 && pressors[i] == 0) {
    circPoint[i] <- 0
  } else if (circFail[i] < 2 && pressors[i] == 0) {
    circPoint[i] <- 1
  } else if (circFail[i] >= 2 && circFail[i] <= 2.5 && pressors[i] == 0) {
    circPoint[i] <- 2
  } else if (circFail[i] > 2.5 || pressors == 1) {
    circPoint[i] <- 3
  #} else (stop("Missing circulation score value")) }
  } else (circFail[i] <- 0) }

## calculate respiratory points
respPoint <- NULL
for (i in 1:length(respFail1)) {
  if (is.nan(respFail1[i]/respFail2[i]) == TRUE) {
    respPoint[i] <- 0
  } else if (respFail1[i]/respFail2[i] > 357 && mechVent[i] == 0) {
    respPoint[i] <- 1
  } else if (respFail1[i]/respFail2[i] > 214 && respFail1[i]/respFail2[i] <= 357 && mechVent[i] == 0) {
    respPoint[i] <- 2
  } else if (respFail1[i]/respFail2[i] <= 214 || mechVent[i] == 1) {
    respPoint[i] <- 3
  #} else (stop("Missing respiratory score value")) }
  } else (respPoint[i] <- 0) }

## calculate final scores
organFailData <- as.data.frame(cbind(liverPoint, kidneyPoint, brainPoint, coagPoint, circPoint, respPoint))
organFailData$score = apply(organFailData, 1, sum)   # sum each column to get score for each patient

## output data
return(organFailData)

}
