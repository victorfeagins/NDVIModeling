source("GOES_Data_Functions.R")
Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/GOES_Data_2021/"
numcores = 9 
SiteCodeBook = "SiteCodeBook.csv"
Today = Sys.Date()
Daysback = 1



#Extracting Data from files ----
SiteCodedf = read.csv(SiteCodeBook)

Latitude = SiteCodedf$Latitude
Longitude = SiteCodedf$Longitude

Dates <- seq(Today - Daysback, Today-1, by="days") 

ptm <- proc.time()
plan(multisession, workers = numcores)


df = Extract_Dataframe_P(Datadirectory, Latitude, Longitude, Dates, average = TRUE)

(Time<- proc.time() - ptm)



#Cleaning Data ----
library(suncalc) #Used to find suntimes
library(lubridate)# Uses to create intervals

GroupIDs <-  function(dataframe){
  #Creates ID Variables for day of year and position
  output <- dataframe %>% 
    mutate(DaySiteID =  str_c(SiteName, year(Time),yday(Time), sep = "_")) #Unique Identifier based on day and site
  return(output)
}

NDVICreate <- function(dataframe){
  
  output <- dataframe %>% 
    mutate(R2 = RadC02 * KappaC02,
           R3 = RadC03 * KappaC03,
           NDVI = (R3-R2)/(R3+R2),#Function from GOES package
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
    mutate(Daytime = interval(nauticalDawn + 1.5*60*60, nauticalDusk - 1.5*60*60)) %>% #Creating interval object with 1.5 hour trim
    select(DaySiteID, Daytime)# Only need DaySiteID and Daytime interval variables
  
  output <- dataframewithNDVI %>%
    full_join(IntervalCode) %>% #Adds DayTime Interval to every row in dataset
    filter(Time %within% Daytime) %>%  #Only observations that are in daytime
    filter(across(ends_with("DQF"), ~ . == 0)) %>% #All Data Quality Flags are equal to zero
    filter(BCM == 0) %>%  #cloud mask is zero
    filter(NDVI > 0 & round(NDVI,digits=4) != 0.6040 & NDVI < 1) #NDVI positive or noisy number
  
  return(output)
}



df.clean <- df %>%
  left_join(SiteCodedf) %>% 
  GroupIDs() %>% 
  NDVICreate() %>% #Creation of NDVI variables
  NDVIQuality() %>%  #Quality needs group variables for applying Daytime interval
  mutate(LocalTZ = lutz::tz_lookup_coords(Latitude, Longitude, warn = FALSE)) %>% #Creating localtimezone variable
  mutate(LocalTime = mapply(format, x = Time, tz = LocalTZ)) #Converting Time into local time zone


# #Exploring LocalTime
# library(ggplot2)
# 
# df.clean %>% 
#   select(LocalTime, NDVI, DaySiteID) %>% 
#   mutate(LocalTime = hour(LocalTime) + minute(LocalTime)/60) %>% #Eventually might need to convert to local time zone
#   group_by(DaySiteID) %>% 
#   filter(n() >= 25) %>% 
#   reshape2::melt(c("LocalTime", "DaySiteID"))  %>%
#   ggplot(mapping = aes(x = LocalTime , y = value)) +
#   geom_point() +
#   facet_wrap(~DaySiteID, scales = "free")+
#   labs(title = "NDVI by DaySiteID", x = "LocalTime (hour)")



df.model.vectors <- df.clean %>% 
  select(LocalTime, NDVI, DaySiteID) %>% 
  mutate(LocalTime = hour(LocalTime) + minute(LocalTime)/60) %>% #
  rename(y = NDVI, x = LocalTime) %>% 
  group_by(DaySiteID) %>% 
  filter(n() >= 25)%>% #Keep onlysite day that have more then 25 obs
  group_split() %>% #Splits groups up into a list
  lapply(as.list) #Diurnal modeling wants lists


