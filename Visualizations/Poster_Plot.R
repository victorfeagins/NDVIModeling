
library(ggplot2)
library(stringr)
library(dplyr)


inputdirectory <- "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/"
#sitecodebook <- "/projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/GOESdownloadSites.csv"

inputfiles <- list.files(inputdirectory)
# 3 is good
# 10
inputfile = inputfiles[[10]]

df <- file.path(inputdirectory, inputfile ) %>% 
  read.csv()

inputfile  %>% 
  str_extract("_[\\d]+_[\\d]+_") %>%
  as.Date("_%Y_%j_")


ggplot(df) +
  geom_point(mapping = aes(x,y)) +
  ggthemes::theme_base() +
  labs(title = "Appalachian State University Raw NDVI Time Series 2020/04/27",
       y = "NDVI",
       x = "Time (Hour)")
