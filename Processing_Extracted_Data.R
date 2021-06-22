library(suncalc)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate) 
library(scales)

df.scale <- read.csv("TestDataOffset.csv") %>% 
  mutate(R2 = RadC02 * KappaC02,
         R3 = RadC03 * KappaC03,
         NDVI = (R3-R2)/(R3+R2))

df.noscale <-  read.csv("TestDataNoOffset.csv") %>% 
  mutate(R2 = RadC02 * KappaC02,
         R3 = RadC03 * KappaC03,
         NDVI = (R3-R2)/(R3+R2))




suntimes<- getSunlightTimes(date = as.Date(df.scale$Time[2]),
                 lat = df.scale$Latitude[2],
                 lon = df.scale$Longitude[2])
                 keep=c("nauticalDawn","nauticalDusk")
suntimes$nauticalDawn

suntimes$nauticalDusk


#lubridate::hour(suntimes$nauticalDawn) + (lubridate::minute(suntimes$nauticalDawn)/60) #Turns it into fraction hours


Daytime.scale <-  df.scale %>% 
  filter(Time > suntimes$nauticalDawn, Time < suntimes$nauticalDusk )

Daytime.noscale <- df.noscale %>% 
  filter(Time > suntimes$nauticalDawn, Time < suntimes$nauticalDusk )

# Plotting NDVI Values vs Time
Daytime.scale  %$% 
  plot(as.POSIXct(Time), NDVI+.4, main = "NDVI Values Scaled")

Daytime.noscale  %$% 
  plot(as.POSIXct(Time), NDVI, main = "NDVI Values Not Scaled")


Daytime.scale %>% 
  filter(NDVI+.4 > 0) %$%
  plot(as.POSIXct(Time), NDVI+.4, main = "NDVI Values Scaled +")

Daytime.noscale %>% 
  filter(NDVI > 0) %$%
  plot(as.POSIXct(Time), NDVI, main = "NDVI Values Not Scaled +")


Daytime.noscale %>% 
  filter(NDVI > 0) %>% 
  nrow()

Daytime.scale %>% 
  filter(NDVI > 0) %>% 
  nrow()


# Plotting R Values vs Time

# All Time -----
Daytime.scale %>% 
  select(Time, R2, R3) %>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  labs(title = "Values Scaled", x = "Time")+
  scale_x_datetime(labels = date_format("%H:%M:%S"))
  

Daytime.noscale %>% 
  select(Time, R2, R3) %>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  labs(title = "Values Not Scaled", x = "Time")+
  scale_x_datetime(labels = date_format("%H:%M:%S"))








GOES_NDVI_Data <- read.csv("GOES_NDVI_DiurnalRussellSage_2017233.csv")
GOES_NDVI_Data <- GOES_NDVI_Data %>% 
  t()
colnames(GOES_NDVI_Data) <- c("NDVI", "Time")
  GOES_NDVI_Data %>% 
    as.data.frame() %$% 
    plot(Time, NDVI, main = )
