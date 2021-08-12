# Learning NetCdf files -----------
# Author: Victor Feagins
# Description: I am learning about the .nc files

## Packages ----
library(ncdf4)
library(ncdf4.helpers)#Used for Climiate data
library(raster)



## Data ----

NC_Test <- nc_open("Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc")



## Exploring Data ----

print(NC_Test)
page(NC_Test, method = "print") #Opens page to explore the insides of the variable
ls.str(NC_Test$var$Rad)
attributes(NC_Test)
attributes(NC_Test$var)
attributes(NC_Test$dim)
attributes(NC_Test$dim$x)

dat <-  ncvar_get(NC_Test,"Rad")
dim(dat)


t <- ncvar_get(NC_Test,"lon")

image(dat[0:100,0:100])

## Trying Raster ----
test_raster <- raster("Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc", 
                      varname="Rad",
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#Raster doen't know the geoinformation from the .nc file


## Trying ncdf.helpers ----

nc.get.coordinate.axes(NC_Test, "Rad")

nc.get.proj4.string(NC_Test, "x")

nc.get.dim.names(NC_Test, "Rad")

nc.get.time.series(NC_Test)

# nc.get.var.subset.by.axes seems useful

nc.get.var.subset.by.axes(NC_Test, "Rad", list(X=1:4, Y=c(1, 3, 5)))

nc.get.coordinate.axes(NC_Test, "Rad")



#nc.get.var.subset.by.axes This one seems to the most useful.



## Where are my variables at -----
#What I want to to select a spatial range and extract values for that range.
attributes(NC_Test)

attributes(NC_Test$var)
attributes(NC_Test$var$Rad)

test = NC_Test$var$Rad
# It has offset and scalefactor does that mean I need to adjust the variables

attributes(NC_Test)
NC_Test$dim
attributes(NC_Test$dim)
