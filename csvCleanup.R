csvCleanup <- function(experimentalData) {
  
  #############################
  ## liverScores package     ##
  ## Kate Wolfer             ##
  ## v1.0, 27 November 2019  ##
  ############################# 
  
  ## quick and dirty clean up of unwanted artefacts 
  ## generally found in Excel files
  
  
  forRemoval <- c("#DIV/0!", 
                  "#NAME?", 
                  "#NULL!", 
                  "#NUM!", 
                  "#REF!", 
                  "#VALUE!",
                  "PENDING")
  
  for (i in 1:ncol(experimentalData)) {
    for (j in 1:nrow(experimentalData)) {
      findRemoval <- grep(paste(forRemoval, collapse="|"), paste(experimentalData[j,i]))
      if (length(findRemoval) > 0) {
        experimentalData[j,i] <- NA
      }
    }
  }
  
  return(experimentalData)
}

