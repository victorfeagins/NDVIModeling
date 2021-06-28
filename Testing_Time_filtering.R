library(lubridate)
library(stringr)



Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/GOES_Data_2020/"
DateDay = "2020-01-01" %>% 
  as.Date() %>% 
  strftime(test, format = "%j")
  
files<- list.files(Datadirectory)



days <- files %>% 
  str_extract("_s.+") %>% 
  str_sub(7,9)

files[days == DateDay] %>% 
  str_extract("_s.+") %>% 
  str_sub(7,9) %>% 
  as.factor() %>% 
  summary()

files.filtered <- files[days == DateDay]
 

Channel2files <-  str_subset(files.filtered, "L1b-RadC-M[\\d]C02_G16")

Channel2time_day <- Channel2files %>% 
  str_extract("_s.+") %>% 
  

Channel2time_day %>% 
  as.factor() %>% 
  summary()

head(Channel2files)
head(Channel2time_day)

#Date to 365 Day ----
(test = as.Date("2020-02-01") %>% 
  strftime(test, format = "%j"))

