#############################
## liverScores package     ##
## Kate Wolfer             ##
## v1.0, 27 November 2019  ##
#############################

expData = "GLA_required_extraction_14Oct2019.csv"
metData = "GLA RECRUITMENT_DROPBOX_CURRENT_14Oct2019.csv"

## import subsetting data
experimentalData <- read.csv(expData, 
                             check.names=FALSE, 
                             stringsAsFactors=FALSE)

## import metadata
metadata <- read.csv(metData, 
                     check.names=FALSE, 
                     stringsAsFactors=FALSE)

## clean metadata before use, add recalculated scores
source("GLAcleanup.R")
metadata <- GLAcleanup(metadata, metData)
metadataSub <- metadata[,which(is.element(colnames(metadata), experimentalData$columns) == TRUE)]
metadataSub <- cbind(metadataSub, metadata[,c(604:612)])

## check ID lists from metadata and required subset overlap
#idList <- intersect(experimentalData$id[c(1:164)], metadata$baseStudyNumber)
#idList <- is.element(experimentalData$id[c(1:164)], metadata$baseStudyNumber)
#experimentalData$id[which(idList == FALSE)]

## final reduction of metadata, reorder
newTable <- NULL
for (i in 1:164){
  findIndex <- match(paste(experimentalData$id[i]), paste(metadataSub$baseStudyNumber))
  if (i == 1){
    newTable <- metadataSub[findIndex,]
  } else {newTable[i,] <- metadataSub[findIndex,] }
}

newTable$baseStudyNumber == experimentalData$id[c(1:164)]

write.csv(newTable,'GLA_required_extraction_14Oct2019_subsetted.csv')


## Make test set of GLA data
findCols <- read.csv("liver scores definitions.csv")
testData <- metadata[,which(is.element(colnames(metadata),findCols$columnName) == TRUE)]
testData <- testData[,match(colnames(testData), findCols$columnName)]
colnames(testData) <- findCols$variableName
testData <- cbind(metadata$baseStudyNumber, testData)
colnames(testData)[1] <- "baseStudyNumber"


write.csv(testData,'GLA_required_extraction_14Oct2019_subsetted.csv')