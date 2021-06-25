
source("GOES_Data_Functions.R")
library(GOESDiurnalNDVI)

Latiude = 32.457 
Longitude = -91.9743 

file = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc"



NC_File <- nc_open(file)
NC_info <- File_info(NC_File)

coords.to.angle(Latiude, Longitude, NC_info)

coords.to.index(Latiude, Longitude, NC_info)


Lat.rad = deg2rad(Latiude)

Long.rad = deg2rad(Longitude)

getABI_Index(Lat.rad, Long.rad, "OLD") %>% 
  getDataIndex(2, "OLD")


