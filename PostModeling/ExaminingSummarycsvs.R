library(stringr)

inputdirectory <- "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/"

outputmodeldirectory <- "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/"

summarymodeldirectory <- "/projectnb/dietzelab/GOES_DataFTP/SummaryModel/"

inputfiles <- list.files(inputdirectory)
outputfiles <- list.files(outputmodeldirectory)

DaySiteID.input <- inputfiles %>% 
  str_remove("_input")

DaySiteID.output <- outputfiles %>% 
  str_remove(".rds") %>% 
  str_remove("_output")


ModelRequest <- inputfiles[!DaySiteID.input %in% DaySiteID.output] #Files that did not model


#Summary Files ----

summaryfiles <- list.files(summarymodeldirectory, full.names = TRUE)

Data <- lapply(summaryfiles, read.csv)

test <- Data[[22]]
