## GLA database adding scoring

# convert the database to csv format and add the data headers to the first row
# so that all the rows stay in the same place and can be more easily copied across

# set the working directory where your files and the GLAcleanup function file
# are:

setwd("C:/myDirectory")

# import the csv file
metadata <- read.csv("GLA RECRUITMENT_DROPBOX_CURRENT_30June2020.csv",
                     check.names=FALSE,
                     stringsAsFactors=FALSE)

# decide on the file name you want the sorted data to be called - can just be
# the same as the input file
fileName <- "GLA RECRUITMENT_DROPBOX_CURRENT_30June2020.csv"

# this is the function needed - open it and run it
source("GLAcleanup.R")
GLA <- GLAcleanup(metadata, fileName)

# then copy the added scores into the original sheet
