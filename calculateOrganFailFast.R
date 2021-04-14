############################################################
## Calculating organ failure scoring for CLIF scores      ##
## Kate Leary, Institute of Hepatology                    ##
## 19 June 2018                                           ##
############################################################

# this function calculates the organ failure score for each patient in the list
# respiratory failure: PaO2/FiO2 or SpO2/FiO2, use latter for GLA

calculateOrganFailFast <- function(liverFail,kidneyFail,kidneyFail2,brainFail,coagFail,circFail,respFail1,respFail2, mechVent, pressors) {

# ## ensure columns are correctly formatted
# liverFail <- as.numeric(paste(liverFail))
# kidneyFail <- as.numeric(paste(kidneyFail))
# kidneyFail2 <- as.numeric(paste(kidneyFail2))
# brainFail <- as.numeric(paste(brainFail))
# coagFail <- as.numeric(paste(coagFail))
# circFail <- as.numeric(paste(circFail))
# respFail1 <- as.numeric(paste(respFail1))
# respFail2 <- as.numeric(paste(respFail2))
# mechVent <- as.numeric(paste(mechVent))
# pressors <- as.numeric(paste(pressors))

## calculate liver points
liverPoint <- liverFail
liverPoint[which(liverFail < 6)] <- 1
liverPoint[which(liverFail >= 6 & liverFail <= 12)] <- 2
liverPoint[which(liverFail > 12)] <- 3


## calculate kidney points
kidneyPoint <- kidneyFail
kidneyPoint[which(kidneyFail > 0 & kidneyFail < 2)] <- 1
kidneyPoint[which(kidneyFail >= 2 & kidneyFail <= 3.5)] <- 2
kidneyPoint[which(kidneyFail > 3.5)] <- 3
kidneyPoint[which(kidneyFail2 == 1)] <- 3

## calculate brain points
brainPoint <- brainFail
brainPoint[which(brainFail == 0)] <- 1
brainPoint[which(brainFail > 0 & brainFail <= 2)] <- 2
brainPoint[which(brainFail > 2)] <- 3

## calculate coagulation points
coagPoint <- coagFail
coagPoint[which(coagFail == 0)] <- 0
coagPoint[which(coagFail > 0 & coagFail < 2)] <- 1
coagPoint[which(coagFail >= 2 & coagFail <= 2.5)] <- 2
coagPoint[which(coagFail > 2.5)] <- 3

## calculate circulation points
circPoint <- circFail
circPoint[which(circFail > 70)] <- 1
circPoint[which(circFail <= 70)] <- 2
circPoint[which(pressors > 0)] <- 3

## calculate respiratory points
respFail <- respFail1/respFail2
mechVent[which(is.na(mechVent) == TRUE)] <- 0
respPoint <- respFail

respPoint[which(respFail > 357)] <- 1
respPoint[which(respFail <= 357 & respFail > 214)] <- 2
respPoint[which(respFail <= 214)] <- 3
respPoint[which(mechVent == 0)] <- 3

## calculate final scores
organFailData <- as.data.frame(cbind(liverPoint, kidneyPoint, brainPoint, coagPoint, circPoint, respPoint))
organFailData$score = apply(organFailData, 1, sum)   # sum each column to get score for each patient

## output data
return(organFailData)

}
