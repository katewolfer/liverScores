pHack <- function(){
  
  ##########################################
  ## pHack R package                      ##
  ## Kate Wolfer, Imperial College London ##
  ## v1.0, 20 August 2019                 ##
  ##########################################
  
  ## required packages
  # require(ggplot2)
  # require(reshape2)
  # require(moments)
  # require(gridExtra)
  # require(shiny)
  
  
  ## Import the basic dataframes
  metadataTitle <- readline(prompt="Enter name of metadata file: ")
  experimentTitle <- readline(prompt="Enter name of experimental data file: ")
  
  metadata <- read.csv(metadataTitle, 
                       check.names=FALSE, 
                       stringsAsFactors=FALSE)
  
  experimentalData <- read.csv(experimentTitle, 
                               check.names = FALSE, 
                               stringsAsFactors=FALSE)
  
  # Match the list of subjects in the metadata list with 
  # the experimental data list, reorder everything
  # 
  source("subsetMetadata.R")
  subMetadata <- subsetMetadata(metadata, experimentalData)
  
  source("orderData.R")
  experimentalData <- orderData(experimentalData)
  
  # check row to row correspondence of both dataframes
  checkMatch <- sum(subMetadata$id == experimentalData$id)
  if(checkMatch == nrow(experimentalData)){
    cat("Row IDs for experimental data and metadata successfully matched")
  } else {
    stop("Row IDs for experimental data and metadata do not match")
  }
  
}

