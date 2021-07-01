#Input ----
numCores = 9
inputdirectory = "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2020/"


outputdirectory = "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2020/"



#Program ----
library(future) #Parallel process
library(future.apply) #future lapply
library(GOESDiurnalNDVI) #Used for running NDVI modeling and calculating NDVI
library(stringr)

DiurnalModeling <- function(Data){
  j.model = createDiurnalModel("Test", Data)
  var.burn <- runMCMC_Model(j.model=j.model,variableNames=c("a","c","k","prec"),
                            baseNum=20000,iterSize =10000)
  out.mat <- as.matrix(var.burn)
  thinAmount <- round(nrow(out.mat)/5000,digits=0)
  var.burn <- window(var.burn,thin=thinAmount)
  attr(var.burn, "DaySiteID") <- unique(Data$DaySiteID) #Going to need to find a more informative id
  
  return(var.burn)
}

#Reading in Data -----
inputfiles = list.files(inputdirectory)

inputID = inputfiles %>% 
  str_replace("_input", "")

outputfiles = list.files(outputdirectory)

outputID = outputfiles %>% 
  str_replace("_output", "")


modelfiles <- inputfiles[!inputID %in% outputID] %>% 
  file.path(inputdirectory, .)


model.list <- lapply(modelfiles, function(x){as.list(read.csv(x))})


#Running Model ----

test <-  model.list[1:numCores]

plan(multisession, workers = numCores)
ptm <- proc.time()
modeloutput<- future_lapply(test, DiurnalModeling)


(Time<- proc.time() - ptm)







