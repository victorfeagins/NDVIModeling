# Learning NetCdf files -----------
# Author: Victor Feagins
# Description: I am learning about the .nc files

## Packages ----
library(ncdf4)
library(raster)


## Data ----

NC_Test <- nc_open("Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc")



## Exploring Data ----

print(NC_Test)
page(NC_Test, method = "print")
ls.str(NC_Test)
attributes(NC_Test)
attributes(NC_Test$var)
attributes(NC_Test$dim)
attributes(NC_Test$dim$x)

dat <-  ncvar_get(NC_Test,"Rad")
dim(dat)


t <- ncvar_get(NC_Test,"lon")

image(dat[0:100,0:100])

test_raster <- raster("Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc", 
                      varname="Rad",
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

