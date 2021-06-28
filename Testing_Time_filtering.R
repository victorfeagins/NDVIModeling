library(lubridate)
library(stringr)



Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/GOES_Data_2020/"

files<- list.files(Datadirectory)

files %>% 
  str_extract("_s.+") %>% 
  nchar() %>% 
  summary()

Channel2files <-  str_subset(files, "L1b-RadC-M[\\d]C02_G16")

Channel2time_day <- Channel2files %>% 
  str_extract("_s.+") %>% 
  str_sub(7,9)

Channel2time_day %>% 
  as.factor() %>% 
  summary()

head(Channel2files)
head(Channel2time_day)

#Date to 365 Day ----
(test = as.Date("2020-02-01") %>% 
  strftime(test, format = "%j"))

