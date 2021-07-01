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


df.siteinfo <- df %>% 
  left_join(SiteCodedf)

#Cleaning Data ----
library(suncalc) #Used to find suntimes
library(lubridate)# Uses to create intervals

GroupIDs <-  function(dataframe){
  #Creates ID Variables for day of year and position
  output <- dataframe %>% 
    mutate(DaySiteID =  str_c(SiteName, year(Time),yday(Time), sep = "_")) #Unique Identifier based on day and site
  return(output)
}

test <- df.siteinfo %>% 
  GroupIDs()

