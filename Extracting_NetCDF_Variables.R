# Extracting Variables From NetCDF -----------
# Author: Victor Feagins
# Description: Extracting from .nc files variables I need to do analysis


## Packages ----
library(ncdf4)
library(ncdf4.helpers)
library(stringr)

## File ----

filename = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc"
#filename = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330207189_e20172330209562_c20172330210002.nc"
#filename = "Data/OR_ABI-L1b-RadC-M3C03_G16_s20172330312189_e20172330314562_c20172330315004.nc"
#filename = "Data/OR_ABI-L2-ACMC-M4_G16_s20172330110227_e20172330110227_c20172330116257.nc"
NC_file <- nc_open(filename)
ls.str(NC_file$var$Rad)
## Variables Extraction ----
#Many important variables are not contained in the NC_File object but only appear when doing print

print.output<- capture.output(NC_file) #Saves the output of print(NC_file)
### Coordinate ----

# Offset and factors found in print(NC_Test) Can't seem to find them using the object NC_file.
#This code assumes that the last 2 pairs of offset and factors are y and x.


Factor_Offset<- print.output %>% 
  str_subset("scale_factor|add_offset") #looking for scale factor or add_offset
LengthFactor_Offset=length(Factor_Offset)


#### Y factor and offset ----
y.scale_factor = Factor_Offset[LengthFactor_Offset-3] %>% 
    str_extract("-?[\\d]+.?[\\d]+e?-?[\\d]+") %>% 
    as.numeric()
y.offset = Factor_Offset[LengthFactor_Offset-2] %>% 
    str_extract("-?[\\d]+.?[\\d]+e?-?[\\d]+") %>% 
    as.numeric()

#### X factor and offset ----
x.scale_factor = Factor_Offset[LengthFactor_Offset-1] %>% 
    str_extract("-?[\\d]+.?[\\d]+e?-?[\\d]+") %>% 
    as.numeric()
x.offset = Factor_Offset[LengthFactor_Offset] %>% 
    str_extract("-?[\\d]+.?[\\d]+e?-?[\\d]+") %>% 
    as.numeric()


#### Getting Longitude and Latiude
#Follow the equations in Volume 3: Level1b products section 5.1.2.8
#Strange_Num <- ncvar_get(NC_file, "goes_imager_projection")



nc_close(NC_file)
### Radiances ----

variable = "Rad"
attributes(NC_file$var$Rad)

# See section 5.1 on Volume 1 GOES R SERIES PRODUCT DEFINITION AND USERS’ GUIDE
Scale_Factor <- NC_file$var[[variable]]$scaleFact
Offset <- NC_file$var[[variable]]$addOffset
Missing_val <- NC_file$var[[variable]]$missval

Kappa = ncvar_get(NC_file,"kappa0")


### Data Quality Flags ----
#See Section 5.1.3.6.4 on Volume 3 Level 1b Products on Data Quality Flag Values
attributes(NC_file$var$DQF)

DataQualityFlag = ncvar_get(NC_file,"DQF")


### Time ----
# See section 5.0.1 of Volume 3 Level 1b products
# Time recorded as seconds from 2000-01-01 12:00:00 UTC
attributes(NC_file$var$time_bounds)
Time_Bounds <-  ncvar_get(NC_file,"time_bounds")
as.POSIXct(test[1], origin = "2000-01-01 12:00:00", tz = "UTC")
as.POSIXct(test[2], origin = "2000-01-01 12:00:00", tz = "UTC")
