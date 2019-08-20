###########################
## pHack package         ##
## Kate Wolfer           ##
## v1.0, 15 August 2019  ##
###########################

## required packages
require(ggplot2)
require(reshape2)
require(moments)
require(gridExtra)
require(shiny)

metadataTitle <- "metadata.csv"
experimentTitle <- "experimental_data.csv"

## import metadata
metadata <- read.csv(metadataTitle, 
                     check.names=FALSE, 
                     stringsAsFactors=FALSE)

## Import data
experimentalData <- read.csv(experimentTitle, 
                             check.names = FALSE, 
                             stringsAsFactors=FALSE)

## setupOutput
# needs setting up internally rather than importing as file
outputFile <- read.csv("output.csv", 
                       check.names = FALSE, 
                       stringsAsFactors=FALSE)


## TO DO:


# 1) Match the list of subjects in the metadata list with the experimental data list

## subset the metadata to match the experimental data list, reorder to ensure 
# rows match

subMetadata <- subsetMetadata(metadata, experimentalData)

# 2) Import details of columns to do stats on


# 3) Find the biofluid type in column headers if present

# STEP 1: match up any biofluids - simplifies plotting later
# user defined additions
source("userKey.R")
userList <- userKey()

# call predefined list, add user inputs
source("getBiofluids.R")
biofluidsList <- getBiofluids()
biofluidsList <- unique(c(userList, biofluidsList))

# get experimental data column headers
fetchColumns <- colnames(experimentalData)

findBiofluidColumns <- matrix(data = FALSE,
                              ncol = ncol(experimentalData), 
                              nrow = length(biofluidsList))

# find biofluids represented in column names
for(findMatch in 1:length(biofluidsList)){
  findOverlap <- grep(biofluidsList[findMatch], fetchColumns)
  findBiofluidColumns[findMatch, findOverlap] <- TRUE
}
getHitsBio <- rowSums(findBiofluidColumns)
representBio <- biofluidsList[which(getHits > 0)]

# STEP 2: match up biofluid pairs
trimColNames <- gsub("[^0-9]", "", fetchColumns)

findPairMatches <- matrix(data = FALSE,
                          ncol = ncol(experimentalData), 
                          nrow = ncol(experimentalData))

# find biofluids represented in column names
for(findMatch in 1:length(biofluidsList)){
  findOverlap <- grep(biofluidsList[findMatch], fetchColumns)
  findBiofluidColumns[findMatch, findOverlap] <- TRUE
}
getHitsAnalyte <- rowSums(findBiofluidColumns)
representAnalyte <- biofluidsList[which(getHits > 0)]







# 4) Calculate liver scores
# 5) Get the correlation etc. statistics for each comparison
# 6) Populate the output file
# 7) Make the stats plot, save to a figure file
# 8) OPTIONAL make an interactive Shiny plot? Save?
# 9) On completion of all comparisons: apply multiple comparisons correction
# 10) Order output file figures by significance etc.
# 11) Check most significant? Highlight issues?
# 12) Output stats file



