library(suncalc)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate) 
library(scales)
library(coda)


#Package from Github
library(GOESDiurnalNDVI)

# Functions ----
NDVICreate <- function(dataframe){
  
  output <- dataframe %>% 
    mutate(R2 = RadC02 * KappaC02,
           R3 = RadC03 * KappaC03,
           NDVI = calNDVI(R2, R3),#Function from GOES package
           Time =  as_datetime(Time))
  return(output)
}

NDVIQuality <- function(dataframewithNDVI){
  
  suntimesinterval<- dataframewithNDVI %$% 
    getSunlightTimes(date = as.Date(Time[1]),
                     lat = Latitude[1],
                     lon = Longitude[1],
                     keep=c("nauticalDawn","nauticalDusk"), tz = "UTC") %$%
    interval(nauticalDawn,nauticalDusk, "UTC") 
  
  
  output <- dataframewithNDVI %>% 
    filter(Time %within% suntimesinterval) %>% # Time is between nauticalDawn & nauticalDusk
    filter(across(ends_with("DQF"), ~ . == 0)) %>% #All Data Quality Flags are equal to zero
    filter(BCM == 0) %>%  #cloud mask is zero
    filter(NDVI > 0 | round(NDVI,digits=4) != 0.6040) #NDVI positive or noisy number
  return(output)
}



# Data ----

df.avg <-  read.csv("TestDataAvg.csv")

GOES_NDVI_Data <- read.csv("GOES_NDVI_DiurnalRussellSage_2017233.csv")

df.avg <- df.avg %>% #Selecting only one day
  mutate(Time = as_datetime(Time)) %>% 
  filter(day(Time) == 21)




#Cleaning Data ----

df.avg.model <- df.avg %>%
  NDVICreate() %>% 
  NDVIQuality()



GOES_NDVI_Data <- GOES_NDVI_Data %>% 
  t()
colnames(GOES_NDVI_Data) <- c("NDVI", "Time")

GOES_NDVI_Data<- GOES_NDVI_Data %>% 
  as.data.frame() %>% 
  filter(!is.na(NDVI))


#Plotting Data ----


df.avg.model %$%
  plot(Time, NDVI, main = "Extract_Variable function: R2 Averaged")


GOES_NDVI_Data %$%
  plot(Time, NDVI, main = "Diurnal Functions")


GOES_NDVI_Data %$%
  summary(NDVI)

df.avg.model %$%
  summary(NDVI)


#Baysian Modeling
df.avg.model %$%
  hour(Time)


df.avg.model %$%
  with_tz(Time, "America/New_York") %>% 
  hour()

df.model.vectors <- df.avg.model %>% 
  select(Time, NDVI) %>% 
  mutate(Time = hour(Time) + minute(Time)/60) %>% 
  rename(x = Time, y = NDVI) %$%
  list(x = x, y = y)


j.model = createDiurnalModel("Test", df.model.vectors)

ptm <- proc.time()
var.burn <- runMCMC_Model(j.model=j.model,variableNames=c("a","c","k","prec"),
                          baseNum=20000,iterSize =10000)

(Time<- proc.time() - ptm) #2597.945 seconds
save(var.burn,file="TestModel")



# plot(x = list(), y = list(), main = "2017-08-21", ylim = c(0, 1), 
#      xlim = c(10, 20), ylab = "NDVI", xlab = "Hour", cex = 2.5)
# 
# xseq <- seq(df.model.vectors$x[1], df.model.vectors$x[length(df.model.vectors$x)], 0.001)
# ecoforecastR::ciEnvelope(xseq, ci[1, ], ci[3, ], col = "lightBlue")
# lines(xseq, ci[2, ], col = "black")
# points(as.numeric(df.model.vectors$x), as.numeric(df.model.vectors$y), pch = 20)

# xseq <- seq(df.model.vectors$x[1], df.model.vectors$x[length(df.model.vectors$x)], length.out = 10000)
# #xseq <- seq(0, 24, length.out = 10000)
# 
# out.mat <- as.data.frame(as.matrix(var.burn))
# samplesize = 10000
# a <- sample(out.mat$a, samplesize, replace = T)
# c <- sample(out.mat$c, samplesize, replace = T)
# k <- sample(out.mat$k, samplesize, replace = T)
# 
# leftvalues = a *(1-exp(-1 * (xseq - k))) + c
# 
# rightvalues = a *(1-exp((xseq - k))) + c
# 
# leftmean = leftvalues[xseq <= k]
# 
# rightmean = rightvalues[xseq > k]
# 
# test = c(leftmean,rightmean)
# 
# length(test)
# 
# plot(xseq, test)
# lines(df.model.vectors$x,df.model.vectors$y, col = "blue")






