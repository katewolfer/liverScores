orderData <- function(experimentalData) {
  
  ##########################################
  ## pHack R package                      ##
  ## Kate Wolfer, Imperial College London ##
  ## v1.0, 20 August 2019                 ##
  ##########################################
  
  ## Input arguments
  # experimentalData: dataframe, contains all subject IDs in column 1 ($id), 
  # all other experimental data in other columns, subjects are in rows
  
  ## Output arguments
  # outputData: metadata input dataframe reordered by ID
  
  #######
  
  ## reorder df
  outputData <- experimentalData[order(experimentalData$id),]
  
  ## return ordered data
  return(outputData)
  
}

