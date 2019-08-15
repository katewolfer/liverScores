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

## import metadata
metadata <- read.csv("metadata.csv", 
                     check.names=FALSE, 
                     stringsAsFactors=FALSE)
subsetMetadata <- metadata

## setupOutput
# needs setting up internally rather than importing as file
outputFile <- read.csv("output.csv", 
                       check.names = FALSE, 
                       stringsAsFactors=FALSE)

## Import data
experimentalData <- read.csv("experimental_data.csv", 
                         check.names = FALSE, 
                         stringsAsFactors=FALSE)

## required metadata
# requiredMetadata <- read.csv("GLA_database_required_columns.csv", 
#                              check.names = FALSE, 
#                              stringsAsFactors=FALSE)

# for calculating scores normally, probably to be removed
# scoresData <- read.csv("GLA_score_columns_plus_scores_all_subset_04July2018.csv", 
#                        check.names = FALSE, 
#                        stringsAsFactors=FALSE)


## TO DO:
# 1) Match the list of subjects in the metadata list with the experimental data list
# 2) Import details of columns to do stats on
# 3) Find the biofluid type in column headers if present
# 4) Calculate liver scores
# 5) Get the correlation etc. statistics for each comparison
# 6) Populate the output file
# 7) Make the stats plot, save to a figure file
# 8) OPTIONAL make an interactive Shiny plot? Save?
# 9) On completion of all comparisons: apply multiple comparisons correction
# 10) Order output file figures by significance etc.
# 11) Check most significant? Highlight issues?
# 12) Output stats file



