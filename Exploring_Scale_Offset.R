library(suncalc)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate) 
library(scales)

df.small <- read.csv("TestData.csv")


df.big <-  read.csv("BigData.csv")

NDVICal <- function(R2,R3){
  (R3-R2)/(R3+R2)
}



df.small <-  df.small %>% 
  mutate(R2 = RadC02 * KappaC02,
         R3 = RadC03 * KappaC03,
         NDVI = NDVICal(R2,R3),
         R2_scaled = ((RadC02 - RadC02Offset)/RadC02ScaleFactor)*KappaC02,
         R3_scaled = ((RadC03 - RadC03Offset)/RadC03ScaleFactor)*KappaC03,
         NDVI_scaled = NDVICal(R2_scaled,R3_scaled),
         R2_scaled_Test = ((RadC02 * RadC02ScaleFactor) + RadC02Offset)* KappaC02,
         R3_scaled_Test = ((RadC03 * RadC03ScaleFactor) + RadC03Offset)* KappaC03,
         NDVI_scaled_Test =  NDVICal(R2_scaled_Test,R3_scaled_Test))





suntimes<- df.small %$% 
  getSunlightTimes(date = as.Date(Time[2]),
                            lat = Latitude[2],
                            lon = Longitude[2],
                            keep=c("nauticalDawn","nauticalDusk"))


Daytime.small <-  df.small %>% 
  filter(Time > suntimes$nauticalDawn, Time < suntimes$nauticalDusk)


# Comparing variables with itself
Daytime.small %>% 
  select(Time, starts_with("R2"))%>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  facet_wrap(~variable, scales = "free")+
  labs(title = "R2", x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))
 



Daytime.small %>% 
  select(Time, starts_with("R3"))%>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  facet_wrap(~variable, scales = "free")+
  labs(title = "R3", x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


Daytime.small %>% 
  select(Time, starts_with("NDVI"))%>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  facet_wrap(~variable, scales = "free")+
  labs(title = "NDVI", x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


#Comparing variables with different variables
Daytime.small %>% 
  select(Time, R2,R3,NDVI)%>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  #facet_wrap(~variable, scales = "free")+
  labs(title = "TrueValue = data", x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


Daytime.small %>% 
  select(Time, ends_with("scaled"))%>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  #facet_wrap(~variable, scales = "free")+
  labs(title = "Truevalue = (data-offset)/scalefactor", x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


Daytime.small %>% 
  select(Time, ends_with("test"))%>% 
  reshape2::melt("Time") %>%
  mutate(Time = as.POSIXct(Time)) %>% 
  ggplot(mapping = aes(x = Time , y = value)) +
  geom_line(aes(group = variable, color = variable)) +
  facet_wrap(~variable, scales = "free")+
  labs(title = "Truevalue = data*scalefactor + offset", x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))




