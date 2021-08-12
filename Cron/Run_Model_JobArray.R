#Inputs ----
inputdirectory = "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/"
outputdirectory = "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/"


aggregatedatadirectory = "/projectnb/dietzelab/GOES_DataFTP/SummaryModel/"

fileindex = as.numeric(commandArgs(TRUE)[1]) #Comes from job array
#fileindex = 1
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

#OutPutModel <- readRDS(file.path(outputdirectory,"asuhighlands_2021_186_output"))


# Extacting Values from MCMC List ----

Model.Values <- MCMCvis::MCMCchains(OutPutModel)

ValueExtraction <-  function(modelvalues){
  
  Means <- apply(modelvalues, 2, mean) %>% 
    setNames(nm = str_c(colnames(modelvalues), ".mean"))
  
  Sd <- apply(modelvalues, 2, sd) %>% 
    setNames(nm = str_c(colnames(modelvalues), ".sd"))
  return(c(Means,Sd))
}



c.quan <- quantile(Model.Values[,"c"], probs = c(.025, .5, .975)) %>% 
  setNames(nm = str_c("c.", names(.)))

DaySiteID <-  attr(OutPutModel, "DaySiteID") %>%
  setNames("DaySiteID")

SiteName <- DaySiteID %>% 
  str_remove("_[_\\d]+$") %>% 
  setNames("Site")

DateISO <-  DaySiteID %>% 
  str_extract("_[_\\d]+$") %>% 
  as.Date(format = "_%Y_%j") %>% 
  setNames("Date")

QualityControl <-  "C" %>% 
  setNames("QualityControl")

Summary <- c(DaySiteID, format_ISO8601(DateISO),SiteName, ValueExtraction(Model.Values), c.quan, QualityControl) %>% 
  as.list() %>% 
  as.data.frame()

# Saving Extracted values ----

CSVfilename = DaySiteID %>% 
  str_remove("_[_\\d]+$") %>% 
  str_c(".csv") %>% 
  file.path(aggregatedatadirectory,.)

write.table(Summary, file = CSVfilename, 
            sep = ",", 
            col.names = !file.exists(CSVfilename), 
            append = T,
            row.names = FALSE)

#test <- read.csv(CSVfilename)
