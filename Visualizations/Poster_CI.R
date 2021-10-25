library(stringr)
library(dplyr)
library(coda)
library(GOESDiurnalNDVI)


library(future)
library(future.apply)



inputdirectory <- "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/"
modeldirectory <- "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/"


set.seed(123)
numCores = 3
options(future.globals.maxSize= 891289600)
plan(multisession, workers = numCores)



inputfiles <- list.files(inputdirectory)
modelfiles <- list.files(modeldirectory)


inputfile = inputfiles[[10]]


inputdata <- file.path(inputdirectory, inputfile ) %>% 
  read.csv()

DaySiteID <- unique(inputdata$DaySiteID)

MCMCfile <- modelfiles %>% 
  str_subset(DaySiteID) %>% #Picking the file from date and site
  file.path(modeldirectory, .) %>% 
  readRDS()



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
plot(x=list(),y=list(),main="Appalachian State University Raw NDVI Time Series 2020/04/27" ,ylim=c(0,1),xlim=c(0,24),ylab="NDVI",xlab="Hour",cex=2.5)
ecoforecastR::ciEnvelope(xseq,ci[,1],ci[,3],col="lightBlue") 
lines(xseq,ci[,2],col="black")

points(inputdata$x, inputdata$y)

print(proc.time() - start)


prediction <- as.data.frame(cbind(xseq, ci)) %>% 
  rename(x = xseq,
         median = `50%`,
         LB = `2.5%`,
         UB = `97.5%`)

ggplot(inputdata) +
  ggthemes::theme_base() +
  labs(title = "Appalachian State University Raw NDVI Time Series 2020/04/27",
       y = "NDVI",
       x = "Time (Hour)") +
  lims(x = c(min(inputdata$x),max(inputdata$x)),
       y = c(min(inputdata$y),max(inputdata$y))) +
  geom_ribbon(data = prediction, mapping = aes(x = x, ymin = LB, ymax = UB), fill = "grey70") +
  geom_point(mapping = aes(x,y)) +
  geom_line(data = prediction, mapping = aes(x, median), col = "red")
  

