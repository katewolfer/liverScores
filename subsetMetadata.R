subsetMetadata <- function(metadata, experimentalData) {
  
  ##########################################
  ## pHack R package                      ##
  ## Kate Wolfer, Imperial College London ##
  ## v1.0, 20 August 2019                 ##
  ##########################################
  
  ## Input arguments
  # metadata: dataframe, contains all subject IDs in column 1 ($id), all other 
  # data in other columns, subjects are in rows
  # experimentalData: dataframe, contains all subject IDs in column 1 ($id), 
  # all other experimental data in other columns, subjects are in rows

  ## Output arguments
  # subMeta: metadata input dataframe, subsetted

  #######
  
  ## check overlap of subject IDs, subset the metadata and reorder both df
  subMeta <- metadata[which(is.element(metadata$id,experimentalData$id)),]
  subMeta <- subMeta[order(subMeta$id),]
  experimentalData <- experimentalData[order(experimentalData$id),]
  
  ## throw error if subsets either do not match length or do not match identifiers
  if ((nrow(subMeta) == nrow(experimentalData)) == FALSE){
    stop("Metadata does not contain all data for experimental subjects")
  }
  if (all(subMeta$id == experimentalData$id)) {
    cat("All metadata and experimental subject data matched")
  } else {
    stop("Metadata does not contain all data for experimental subjects")
  }
  
  ## return subsetted data
  return(subMeta)
  
}

