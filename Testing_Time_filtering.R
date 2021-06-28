library(stringr)



Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/GOES_Data_2020/"
DateDay = c("2020-01-01", "2020-01-02", "2020-01-03") %>% 
  as.Date() %>% 
  strftime(test, format = "%Y%j")
  
files<- list.files(Datadirectory)



days <- files %>% 
  str_extract("_s.+") %>% 
  str_sub(3,9)

files[days %in% DateDay] %>% 
  str_extract("_s.+") %>% 
  str_sub(3,9) %>% 
  as.factor() %>% 
  summary()

files.filtered <- files[days %in% DateDay]
 

Channel2files <-  str_subset(files.filtered, "L1b-RadC-M[\\d]C02_G16")

Channel2time_day <- Channel2files %>% 
  str_extract("_s.+") %>% 
  str_sub(3,9)
  

Channel2time_day %>% 
  as.factor() %>% 
  summary()

head(Channel2files)
head(Channel2time_day)

#Date to 365 Day ----
(test = as.Date("2020-02-01") %>% 
  strftime(test, format = "%j"))

# Trying Interval ----
seq(as.Date("2011-12-30"), as.Date("2012-01-04"), by="days") %>% 
  strftime(test, format = "%j")



