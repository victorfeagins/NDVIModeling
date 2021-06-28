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
  str_extract("_s.+")

head(Channel2files)
head(Channel2time_day)






Channel3files <- str_subset(files, "L1b-RadC-M[\\d]C03_G16")
head(Channel3files)

CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC")
head(CloudMask)



  
