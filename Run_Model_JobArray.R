#Inputs ----
inputdirectory = "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/"
outputdirectory = "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/"


fileindex = as.numeric(commandArgs(TRUE)[1]) #Comes from job array
Daysback = 1

#Packages ----
library(lubridate)
library(GOESDiurnalNDVI) #Used for running NDVI modeling and calculating NDVI
library(stringr)

# Functions ----
DiurnalModeling <- function(Data){
  ModelVectors = list(x = Data$x, y = Data$y)
  j.model = createDiurnalModel("Test", ModelVectors)
  var.burn <- runMCMC_Model(j.model=j.model,variableNames=c("a","c","k","prec"),
                              baseNum=20000,iterSize =10000)
  out.mat <- as.matrix(var.burn)
  thinAmount <- round(nrow(out.mat)/5000,digits=0)
  var.burn <- window(var.burn,thin=thinAmount)
  attr(var.burn, "DaySiteID") <- unique(Data$DaySiteID)
  return(var.burn) 
}

#Extracting single file ----
Today = Sys.Date()


Dates <- seq(Today - Daysback, Today-1, by="days")

inputfiles = list.files(inputdirectory) %>% 
  str_subset(str_c(year(Dates),yday(Dates), sep = "_"))


file = inputfiles[fileindex]

Data = file.path(inputdirectory, file) %>% 
  read.csv() %>% 
  as.list()


#Modeling Single file ----
ptm <- proc.time()


OutPutModel <- DiurnalModeling(Data)

print(proc.time() - ptm)

#Saving MCMC List ----


outputfilename = file %>% 
  str_replace("_input", "_output") %>%
  file.path(outputdirectory, .) %>% 
  str_c(".rds")

#When saving with RDS R forgets the original name which is better in this case
#Because later we can rename the object with a better name
saveRDS(OutPutModel, file = outputfilename)

#test <- readRDS("asuhighlands_2021_186_output")

