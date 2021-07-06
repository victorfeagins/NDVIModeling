inputdirectory = "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/"

outputdirectory = "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/"

library(lubridate)
library(GOESDiurnalNDVI) #Used for running NDVI modeling and calculating NDVI
library(stringr)


Today = Sys.Date()
Daysback = 1

Dates <- seq(Today - Daysback, Today-1, by="days")

inputfiles = list.files(inputdirectory)

