library(stringr)
library(dplyr)
library(coda)
library(GOESDiurnalNDVI)


library(future)
library(future.apply)

inputdirectory <- "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/"
modeldirectory <- "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/"

sitecodebook <- "/projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/GOESdownloadSites.csv"


set.seed(123)
numCores = 3
options(future.globals.maxSize= 891289600)
plan(multisession, workers = numCores)



Sites <- read.csv(sitecodebook)

sitenames <- Sites$siteName

modelfiles <- list.files(modeldirectory)
inputfiles <- list.files(inputdirectory)


site = sitenames[1] #Siteselection


dateselection <- modelfiles %>% 
  str_subset(site)%>% #Sitename
  str_extract("_[\\d]+_[\\d]+_") %>%
  as.Date("_%Y_%j_")



date <- dateselection[1]


MCMCfile <- modelfiles %>% 
  str_subset(format(date, str_c(site,"_%Y_%j_"))) %>% #Picking the file from date and site
  file.path(modeldirectory, .) %>% 
  readRDS()

inputdata <- inputfiles %>% 
  str_subset(format(date, str_c(site,"_%Y_%j_"))) %>% #Picking the file from date and site
  file.path(inputdirectory, .) %>% 
  read.csv()

out.mat <- as.matrix(MCMCfile) #need coda loaded for this work properly
rndNums <-  sample.int(nrow(out.mat), 10000, replace = T)

xseq <- seq(0,24,.001)

a <- out.mat[rndNums,1]
c <- out.mat[rndNums,2]
k <- out.mat[rndNums,3]

start <- proc.time()
test <- future_mapply(diurnalExp, a, c, k, MoreArgs = list(xseq = xseq)) #Each column is one possible NDVI time series

print(proc.time() - start)


start <- proc.time()
ci <- future_apply(test,1,quantile,c(0.025,0.5, 0.975), na.rm= TRUE) %>% 
  t()

print(proc.time() - start)

start <- proc.time()
plot(x=list(),y=list(),main=str_c(site,date),ylim=c(0,1),xlim=c(0,24),ylab="NDVI",xlab="Hour",cex=2.5)
ecoforecastR::ciEnvelope(xseq,ci[,1],ci[,3],col="lightBlue") 
lines(xseq,ci[,2],col="black")

points(inputdata$x, inputdata$y)

print(proc.time() - start)

diurnalplot <- function(MCMCFile, RawData){
  out.mat <- as.matrix(MCMCFile) #need coda loaded for this work properly
  rndNums <-  sample.int(nrow(out.mat), 10000, replace = T)
  
  xseq <- seq(0,24,.001)
  
  a <- out.mat[rndNums,1]
  c <- out.mat[rndNums,2]
  k <- out.mat[rndNums,3]
  
  
  values <- mapply(diurnalExp, a, c, k, MoreArgs = list(xseq = xseq)) #Each column is one possible NDVI time series
  
  
  
  ci <- apply(values,1,quantile,c(0.025,0.5, 0.975), na.rm= TRUE) %>% 
    t()
  
  
  plot(x=list(),y=list(),main=str_c(site,date),ylim=c(0,1),xlim=c(0,24),ylab="NDVI",xlab="Hour",cex=2.5)
  ecoforecastR::ciEnvelope(xseq,ci[,1],ci[,3],col="lightBlue") 
  lines(xseq,ci[,2],col="black")
  
  points(RawData$x, RawData$y)
  
}

#diurnalplot(MCMCfile, inputdata)


