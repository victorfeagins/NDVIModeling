library(suncalc)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate) 
library(scales)


#Package from Github
library(GOESDiurnalNDVI)


df.small <- read.csv("TestData.csv")

df.small <- df.small %>% #Selecting only one day
  mutate(Time = as_datetime(Time)) %>% 
  filter(day(Time) == 21)


suntimesinterval<- df.small %$% 
  getSunlightTimes(date = as.Date(Time[1]),
                   lat = Latitude[1],
                   lon = Longitude[1],
                   keep=c("nauticalDawn","nauticalDusk"), tz = "UTC") %$%
  interval(nauticalDawn,nauticalDusk, "UTC") 



df.modeling <- df.small %>% 
  mutate(R2 = RadC02 * KappaC02,
         R3 = RadC03 * KappaC03,
         NDVI = calNDVI(R2, R3),#Function from GOES package
         Time =  as_datetime(Time)) %>% 
  filter(Time %within% suntimesinterval) %>% # Time is between nauticalDawn & nauticalDusk
  filter(across(ends_with("DQF"), ~ . == 0)) %>% #All Data Quality Flags are equal to zero
  filter(BCM == 0) #cloud mask is zero



GOES_NDVI_Data <- read.csv("GOES_NDVI_DiurnalRussellSage_2017233.csv")
GOES_NDVI_Data <- GOES_NDVI_Data %>% 
  t()
colnames(GOES_NDVI_Data) <- c("NDVI", "Time")

GOES_NDVI_Data<- GOES_NDVI_Data %>% 
  as.data.frame() %>% 
  filter(!is.na(NDVI))


df.modeling %$%
  plot(Time, NDVI, main = "Victor's functions")

GOES_NDVI_Data %$%
  plot(Time, NDVI, main = "Diurnal Functions")

df.modeling %$%
  summary(NDVI)

GOES_NDVI_Data %$%
  summary(NDVI)




