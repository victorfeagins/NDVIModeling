
source("GOES_Data_Functions.R")
library(GOESDiurnalNDVI)

Latiude = 32.457 
Longitude = -91.9743 



# Channel 2 ----
file = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc"



NC_File <- nc_open(file)
NC_info <- File_info(NC_File)

coords.to.angle(Latiude, Longitude, NC_info)

Ch2.index<- coords.to.index(Latiude, Longitude, NC_info)


Lat.rad = deg2rad(Latiude)

Long.rad = deg2rad(Longitude)

getABI_Index(Lat.rad, Long.rad, "OLD") %>% 
  getDataIndex(2, "OLD")


## Subset index vs total index

R2 <- ncvar_get(NC_File, "Rad")
R2[Ch2.index$y.index, Ch2.index$x.index]

nc.get.var.subset.by.axes(NC_File, "Rad", list(X=Ch2.index$y.index, Y=Ch2.index$x.index))
nc.get.var.subset.by.axes(NC_File, "Rad", list(X=Ch2.index$x.index, Y=Ch2.index$y.index))



nc_close(NC_File)

# Channel 3 -----

file = "Data/OR_ABI-L1b-RadC-M4C03_G16_s20172330030227_e20172330035029_c20172330035058.nc"
NC_File <- nc_open(file)
NC_info <- File_info(NC_File)

coords.to.angle(Latiude, Longitude, NC_info)

coords.to.index(Latiude, Longitude, NC_info)


getABI_Index(Lat.rad, Long.rad, "OLD") %>% 
  getDataIndex(3, "OLD")


nc_close(NC_File)


# Cloud ------

file = "Data/OR_ABI-L2-ACMC-M4_G16_s20172330145227_e20172330145227_c20172330151279.nc"
NC_File <- nc_open(file)
NC_info <- File_info(NC_File)

coords.to.angle(Latiude, Longitude, NC_info)

coords.to.index(Latiude, Longitude, NC_info)


getABI_Index(Lat.rad, Long.rad, "OLD") %>% 
  getDataIndex("ACM", "OLD")


nc_close(NC_File)




