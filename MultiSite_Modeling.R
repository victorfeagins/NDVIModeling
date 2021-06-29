#Input ----

csvfile = "/projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/MultiSiteJan_01_07.csv"
numCores = 9

#Packages ----
library(suncalc) #Used to find suntimes
library(dplyr) #Used to manuiplate dataframes
library(magrittr) #Used for pipes
library(ggplot2) #Used for graphing
library(lubridate)# Uses to create intervals
library(scales) #Used to create nice axis
library(GOESDiurnalNDVI) #Used for running NDVI modeling and calculating NDVI

library(future)
library(future.apply)


#Functions ----
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

DiurnalModeling <- function(Data){
  j.model = createDiurnalModel("Test", Data)
  var.burn <- runMCMC_Model(j.model=j.model,variableNames=c("a","c","k","prec"),
                            baseNum=20000,iterSize =10000)
  attr(var.burn, "DaySiteID") <- Data$DaySiteID
  
  return(var.burn)
}


# Reading in data ----
df <- read.csv(csvfile)


# Cleaning data ----
df.clean <- df %>% 
  GroupIDs() %>% 
  NDVICreate() %>% 
  NDVIQuality() #Quality needs group variables for applying Daytime


# Preparing data for modeling ----
df.model.vectors <- df.clean %>% 
  select(Time, NDVI, DaySiteID) %>% 
  mutate(Time = hour(Time) + minute(Time)/60) %>% #Eventually might need to convert to local time zone
  rename(x = Time, y = NDVI) %>% 
  group_by(DaySiteID) %>% 
  filter(n() >= 25)%>% #Keep onlysite day that have more then 25 obs
  group_split() %>% #Splits groups up into a list
  lapply(as.list) #Diurnal modeling wants lists

 





# library(parallel)
# 
# 
# ptm <- proc.time()
# 
# modeloutput = mclapply(df.model.vectors, DiurnalModeling, mc.cores = 9)
# (Time<- proc.time() - ptm) #2597.945 seconds


test <- df.model.vectors[1:numCores] #I don't want to wait 2 hours or more so just testing

plan(multisession, workers = numCores)
ptm <- proc.time()
future_lapply(test, DiurnalModeling)


(Time<- proc.time() - ptm) #2597.945 seconds
#save(modeloutput, file ="TestModel6sites_days")



