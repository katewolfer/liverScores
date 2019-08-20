getBiofluids <- function() {
  ##########################################
  ## pHack R package                      ##
  ## Kate Wolfer, Imperial College London ##
  ## v1.0, 20 August 2019                 ##
  ##########################################
  
  # call predefined list of biofluids - separate function so adjusting is easier
  
  biofluidsList <- c("urine", 
                     "stool", 
                     "fecal", 
                     "blood", 
                     "plasma", 
                     "serum", 
                     "bile", 
                     "csf", 
                     "tissue")
  
  return(biofluidsList)
  
}

