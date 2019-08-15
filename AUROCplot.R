AUROCplot <- function(values, classification)

## check length of each input and error if lengths do not match exactly
getLengthVals <- length(values)
getLengthClass <- length(classification)

if(getLengthVals != getLengthClass){
  stop("input values and classifier are not the same length - do you have data for all your samples?")
}

## create data frame for ROC creation from inputs
ps <- data.frame(cbind(values, classification))
ps <- ps[getLengthClass:1,]
colnames(ps) <- c("prediction", "actual")
ps$prediction <- as.numeric(paste(ps$prediction))


thresholds <- seq(59656, 194429, by = 1347.73)

thresholds <- seq(0, 194429, by = 1944.29)
rocPoints <- data.frame(thresholds,
                        fpr = NA,
                        tpr = NA)
rocPoints$thresholds <- as.numeric(paste(rocPoints$thresholds))

nPos = sum(ursocholanic$ACLF)   # Total actual positive for calculating tpr
nNeg = sum(ursocholanic$non)   # Total actual negative for calculating fpr

for (i in seq_along(rocPoints$thresholds)) {
  threshold <- rocPoints$thresholds[i] # start at 1 + step, work down
  tp <- nrow(ps[ps$actual == 1 & ps$prediction >= threshold, ])
  fp <- nrow(ps[ps$actual == 0 & ps$prediction >= threshold, ])
  rocPoints[i, "fpr"] <- fp / nNeg
  rocPoints[i, "tpr"] <- tp / nPos
}

plot(rocPoints$fpr, rocPoints$tpr, xlim=c(0,1), ylim=c(0,1), xlab="FPR", ylab="TPR", col = "blue", pch=19, main = "Ursocholanic acid, ROC for ACLF/cirrosis discrimination")
#lines(rocPoints$fpr, rocPoints$tpr)
#plot(c(0,1), c(0,1))
lines(c(0,1), c(0,1), col = "red")
text(0.8, 0.2, "AUROC = 0.7968254")

simple_auc <- function(TPR, FPR){
  +     # inputs already sorted, best scores first 
    +     dFPR <- c(diff(FPR), 0)
    +     dTPR <- c(diff(TPR), 0)
    +     sum(TPR * dFPR) + sum(dTPR * dFPR)/2
    + }
simple_auc(rocPoints$tpr, rocPoints$fpr)

library(pROC)
roc_obj <- roc(ps$actual, ps$prediction)
auc(roc_obj)