userKey <- function() {
  
  ##########################################
  ## pHack R package                      ##
  ## Kate Wolfer, Imperial College London ##
  ## v1.0, 20 August 2019                 ##
  ##########################################
  
  getTotal <- readline(prompt="Number of different biofluid names to be added: ")
  x <- 0
  userList <- vector()
  while(x < getTotal) {
    addLine <- readline(prompt="Input biofluid to be added to predefined list: ")
    userList = c(userList,addLine)
    x <- x+1
  } 
  
  return(userList)
  
}