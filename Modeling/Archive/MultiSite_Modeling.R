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

library(future) #Parallel process
library(future.apply) #future lapply


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
    filter(NDVI > 0 & round(NDVI,digits=4) != 0.6040 & NDVI < 1) #NDVI positive or noisy number
  
  return(output)
}

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


# Reading in data ----
df <- read.csv(csvfile)


# Cleaning data ----
df.clean <- df %>% 
  GroupIDs() %>% 
  NDVICreate() %>% #Creation of NDVI variables
  NDVIQuality() #Quality needs group variables for applying Daytime



#Exploring Data ----
df.clean %>% 
  select(Time, NDVI, DaySiteID) %>% 
  reshape2::melt(c("Time", "DaySiteID"))  %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_point() +
  facet_wrap(~DaySiteID, scales = "free")+
  labs(title = "NDVI", x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


## Data that will end up in the model ----
df.clean %>% 
  select(Time, NDVI, DaySiteID) %>% 
  mutate(Time = hour(Time) + minute(Time)/60) %>% #Eventually might need to convert to local time zone
  group_by(DaySiteID) %>% 
  filter(n() >= 25) %>% 
  reshape2::melt(c("Time", "DaySiteID"))  %>%
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_point() +
  facet_wrap(~DaySiteID, scales = "free")+
  labs(title = "NDVI by DaySiteID", x = "Time")


# Preparing data for modeling ----
df.model.vectors <- df.clean %>% 
  select(Time, NDVI, DaySiteID) %>% 
  mutate(Time = hour(Time) + minute(Time)/60) %>% #Eventually might need to convert to local time zone
  rename(y = NDVI, x = Time) %>% 
  group_by(DaySiteID) %>% 
  filter(n() >= 25)%>% #Keep onlysite day that have more then 25 obs
  group_split() %>% #Splits groups up into a list
  lapply(as.list) #Diurnal modeling wants lists

 





test <- df.model.vectors[1:numCores] #I don't want to wait 2 hours or more so just testing

plan(multisession, workers = numCores)
ptm <- proc.time()
modeloutput<- future_lapply(test, DiurnalModeling)


(Time<- proc.time() - ptm) #2597.945 seconds

load("TestModel6sites_days")

str(modeloutput[[1]])


#save(modeloutput, file ="TestModel6sites_days")


