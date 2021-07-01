source("GOES_Data_Functions.R")
Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/GOES_Data_2021/"
numcores = 9 
SiteCodeBook = "SiteCodeBook.csv"
Today = Sys.Date()
Daysback = 1

SiteCodedf = read.csv(SiteCodeBook)

Latitude = SiteCodedf$Latitude
Longitude = SiteCodedf$Longitude

Dates <- seq(Today - Daysback, Today-1, by="days") 

ptm <- proc.time()
#plan(multisession, workers = numcores)

plan(sequential)
df = Extract_Dataframe_P(Datadirectory, Latitude, Longitude, Dates, average = TRUE)

(Time<- proc.time() - ptm)


df.siteinfo <- df %>% 
  left_join(SiteCodedf)
