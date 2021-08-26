source("/projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/RawDataProcessing/GOES_Data_Functions.R")

#Input ----
Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/GOES_Data_2021/" #Folder that contains the raw netcdf files
numCores <- as.numeric(commandArgs(TRUE)[1])
#numCores <- 4
SiteCodeBook = "/projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Cron/GOESdownloadSites.csv" #File that is a csv of sites
aggregatedatadirectory = "/projectnb/dietzelab/GOES_DataFTP/SummaryModel/" #Where the summary daily csv's are located

Today = Sys.Date()
Daysback = 1


#Output ---
outputdirectory = "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/" #Where the csv daily files are located. Currently has 2021 and 2020 files


#Extracting Data from files ----
SiteCodedf = read.csv(SiteCodeBook)

Latitude = SiteCodedf$Lat
Longitude = SiteCodedf$Long

Dates <- seq(Today - Daysback, Today-1, by="days") 

ptm <- proc.time()
plan(multisession, workers = numCores)
#plan(sequential)



df = Extract_Dataframe_P(Datadirectory, Latitude, Longitude, Dates, average = TRUE)

(Time<- proc.time() - ptm)



#Cleaning Data ----
library(suncalc) #Used to find suntimes
library(lubridate)# Uses to create intervals

GroupIDs <-  function(dataframe){
  #Creates ID Variables for day of year and position
  output <- dataframe %>% 
    mutate(DaySiteID =  str_c(siteName, year(Time),yday(Time), sep = "_")) #Unique Identifier based on day and site
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

#Cleaning Data----
df.clean <- df %>%
  left_join(SiteCodedf, by = c("Latitude" = "Lat", "Longitude" = "Long")) %>% 
  GroupIDs() %>% #Creation of DaySiteID
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





#Creating Model Vectors ----
df.model.vectors <- df.clean %>% 
  select(LocalTime, NDVI, DaySiteID) %>% 
  mutate(LocalTime = hour(LocalTime) + minute(LocalTime)/60) %>% #
  rename(y = NDVI, x = LocalTime) %>% 
  group_by(DaySiteID) %>% 
  filter(n() >= 25)%>% #Keep onlysite day that have more then 25 obs
  group_split() %>% #Splits groups up into a list
  lapply(as.data.frame) #For saving inputfiles dataframes is easier



#Saving Input Files to Model ----
Outputfilename <- function(df.model.item){
  str_c(unique(df.model.item$DaySiteID), "input", sep = "_")
}


OutputFileNames  <-  map_chr(df.model.vectors, Outputfilename) %>% 
  file.path(outputdirectory,.)


mapply(write.csv, df.model.vectors, file = OutputFileNames, row.names = FALSE)



#Grabbing sites that have insufficient sample size ----

df.nosample <- df.clean %>% 
  mutate(Date = as.Date(LocalTime)) %>%
  mutate(Site = str_remove(DaySiteID, "_[_\\d]+$")) %>% 
  group_by(DaySiteID) %>% 
  filter(n() < 25) %>%
  select(DaySiteID, Date, Site) %>% 
  mutate(a.mean = NA, #Order of this mutate matters since when writing to csv it has to match the index of the old columns.
         c.mean = NA,
         k.mean = NA,
         prec.mean = NA,
         a.sd = NA,
         c.sd = NA,
         k.sd = NA,
         prec.sd = NA,
         c.2.5. = NA,
         c.50. = NA,
         c.97.5. = NA,
         QualityControl = "LS") %>% 
  distinct() %>% 
  group_split()

#Writing nosample to csvs ----
SaveNS <- function(csv, directory){
  CSVfilename <- csv$Site %>% 
    str_c(".csv") %>% 
    file.path(directory,.)
  
  write.table(csv, file = CSVfilename, 
              sep = ",", 
              col.names = !file.exists(CSVfilename), 
              append = T,
              row.names = FALSE)
  
}


lapply(df.nosample, SaveNS, directory = aggregatedatadirectory) #adds rows 

