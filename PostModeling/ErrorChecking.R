library(stringr)


summarydirectory <- "/projectnb/dietzelab/GOES_DataFTP/SummaryModel" #The summary files
inputdirectory <- "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/" #This is the raw data points
modeldirectory <- "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/" #This is the mcmc output

sitecodebook <- "/projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/GOESdownloadSites.csv"

modelfiles <- list.files(modeldirectory)
inputfiles <- list.files(inputdirectory)



Date <- as.Date("2020/01/09")

inputfiles %>% 
  str_subset(format(Date, "NEON.D11.CLBJ.DP1.00033_%Y_%j"))

inputfiles %>% 
  str_subset("NEON.D11.CLBJ.DP1.00033")

%>% #Picking the file from date and site
  file.path(inputdirectory, .) %>% 
  read.csv()

