library(suncalc)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate) 
library(scales)


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
df.small <- read.csv("TestData.csv")

df.avg <-  read.csv("TestDataAvg.csv")

df.avg.fun <-  read.csv("TestDataAvgFunction.csv")

GOES_NDVI_Data <- read.csv("GOES_NDVI_DiurnalRussellSage_2017233.csv")

df.small <- df.small %>% #Selecting only one day
  mutate(Time = as_datetime(Time)) %>% 
  filter(day(Time) == 21)

df.avg <- df.avg %>% #Selecting only one day
  mutate(Time = as_datetime(Time)) %>% 
  filter(day(Time) == 21)

df.avg.fun <- df.avg.fun %>% #Selecting only one day
  mutate(Time = as_datetime(Time)) %>% 
  filter(day(Time) == 21)


#Cleaning Data ----
df.small.model <-  NDVICreate(df.small) %>% 
  NDVIQuality()

df.avg.model <-  NDVICreate(df.avg) %>% 
  NDVIQuality()

df.avg.fun.raw <- NDVICreate(df.avg.fun) 
df.avg.fun.model <- df.avg.fun.raw %>% 
  NDVIQuality()

GOES_NDVI_Data <- GOES_NDVI_Data %>% 
  t()
colnames(GOES_NDVI_Data) <- c("NDVI", "Time")

GOES_NDVI_Data<- GOES_NDVI_Data %>% 
  as.data.frame() %>% 
  filter(!is.na(NDVI))


#Ploting Data ----

df.small.model %$%
  plot(Time, NDVI, main = "Extract_Variable function")


df.avg.model %$%
  plot(Time, NDVI, main = "Extract_Variable function: R2 Averaged")

df.avg.fun.model %$%
  plot(Time, NDVI, main = "Extract_Variable function: R2 Averaged SmallWindow")


GOES_NDVI_Data %$%
  plot(Time, NDVI, main = "Diurnal Functions")

df.small.model %$%
  summary(NDVI)

GOES_NDVI_Data %$%
  summary(NDVI)

df.avg.model %$%
  summary(NDVI)

df.avg.fun.model %$%
  summary(NDVI)

#Comparing Raw Data to GOES_NDVI_Data ----

suntimesinterval<- df.avg.fun.raw %$% 
  getSunlightTimes(date = as.Date(Time[1]),
                   lat = Latitude[1],
                   lon = Longitude[1],
                   keep=c("nauticalDawn","nauticalDusk"), tz = "UTC") %$%
  interval(nauticalDawn,nauticalDusk, "UTC") 

df.avg.fun.raw %>% 
  filter(Time %within% suntimesinterval,
         NDVI > 0) %>% 
  mutate(Time = hour(with_tz(Time, tzone = "EDT")) + minute(with_tz(Time, tzone = "EDT"))/60)%$%
  plot(Time,NDVI)

GOES_NDVI_Data %$%
  plot(Time, NDVI, main = "Diurnal Functions")


