library(suncalc)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate) 
library(scales)

library(GOESDiurnalNDVI)

GroupIDs <-  function(dataframe){
  #Creates ID Variables for day of year and position
  output <- dataframe %>% 
    mutate(SiteID = group_indices(group_by(., Latitude, Longitude))) %>% #Unique Identifier based on position
    mutate(DaySiteID = group_indices(group_by(., SiteID, yday(Time)))) #Unique Identifier based on day and site
  
  return(output)
}

NDVICreate <- function(dataframe){
  
  output <- dataframe %>% 
    mutate(R2 = RadC02 * KappaC02,
           R3 = RadC03 * KappaC03,
           NDVI = calNDVI(R2, R3),#Function from GOES package
           Time =  as_datetime(Time))
  return(output)
}

NDVIQuality <- function(dataframewithNDVI){
  
  IntervalCode <- dataframewithNDVI %>% 
    mutate(Date = as.Date(Time)) %>% #getSunlightTimes needs date
    select(Date ,Longitude, Latitude, DaySiteID) %>% 
    distinct() %>% #Only need one row per group
    rename(date = Date, lat = Latitude, lon = Longitude) %>% #Renaming columns for getSunlightTimes
    mutate(getSunlightTimes(data = ., keep=c("nauticalDawn","nauticalDusk"), tz = "UTC")) %>% #Mutate adds nauticalDawn and Dusk to df
    mutate(Daytime = interval(nauticalDawn, nauticalDusk)) %>% #Creating interval object
    select(DaySiteID, Daytime)# Only need DaySiteID and Daytime interval variables
  
  output <- dataframewithNDVI %>%
    full_join(IntervalCode) %>% #Adds DayTime Interval to every row in dataset
    filter(Time %within% Daytime) %>%  #Only observations that are in daytime
    filter(across(ends_with("DQF"), ~ . == 0)) %>% #All Data Quality Flags are equal to zero
    filter(BCM == 0) %>%  #cloud mask is zero
    filter(NDVI > 0 & round(NDVI,digits=4) != 0.6040) #NDVI positive or noisy number
  
  return(output)
}

df <- read.csv("MultiSiteJan_01_07.csv")

df.clean <- df %>% 
  GroupIDs() %>% 
  NDVICreate() %>% 
  NDVIQuality() #Quality needs group variables for Daytime



df.model.vectors <- df.clean %>% 
  select(Time, NDVI, DaySiteID) %>% 
  mutate(Time = hour(Time) + minute(Time)/60) %>% 
  rename(x = Time, y = NDVI) %>% 
  group_by(DaySiteID) %>% 
  filter(n() >= 50)%>% 
  group_split() %>% 
  lapply(as.list)

 
DiurnalModeling <- function(Data){
  j.model = createDiurnalModel("Test", Data)
  var.burn <- runMCMC_Model(j.model=j.model,variableNames=c("a","c","k","prec"),
                            baseNum=20000,iterSize =10000)
  attr(var.burn, "DaySiteID") <- Data$DaySiteID
  
  return(var.burn)
}

library(parallel)


ptm <- proc.time()

modeloutput = mclapply(df.model.vectors, DiurnalModeling, mc.cores = 9)
(Time<- proc.time() - ptm) #2597.945 seconds





