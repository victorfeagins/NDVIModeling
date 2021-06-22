library(suncalc)
library(dplyr)
library(magrittr)

df <- read.csv("TestData.csv")


suntimes<- getSunlightTimes(date = as.Date(df$Time[2]),
                 lat = df$Latitude[2],
                 lon = df$Longitude[2],
                 keep=c("nauticalDawn","nauticalDusk"))
suntimes$nauticalDawn

suntimes$nauticalDusk


lubridate::hour(suntimes$nauticalDawn) + (lubridate::minute(suntimes$nauticalDawn)/60) #Turns it into fraction hours


Daytime<- df %>% 
  filter(Time > suntimes$nauticalDawn, Time < suntimes$nauticalDusk )

Daytime <- Daytime %>% 
  mutate(R2 = RadC02 * KappaC02,
         R3 = RadC03 * KappaC03,
         NDVI = (R3-R2)/(R3+R2))

Daytime %>% 
  filter(NDVI > 0) %$%
  plot(as.POSIXct(Time), NDVI)

GOES_NDVI_Data <- read.csv("GOES_NDVI_DiurnalRussellSage_2017233.csv")
GOES_NDVI_Data <- GOES_NDVI_Data %>% 
  t()
colnames(GOES_NDVI_Data) <- c("NDVI", "Time")
  GOES_NDVI_Data %>% 
    as.data.frame() %$% 
    plot(Time, NDVI)
