# Extracting Variables From NetCDF -----------
# Author: Victor Feagins
# Description: Extracting from .nc files variables I need to do analysis


## Packages ----
library(ncdf4)
library(ncdf4.helpers)
library(stringr)
library(magrittr)

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
number_pattern <- "-?[\\d]+.?[\\d]*e?-?[\\d]+" #regex expression for grabbing numbers
### Coordinate ----

# Offset and factors found in print(NC_Test) Can't seem to find them using the object NC_file.
#This code assumes that the last 2 pairs of offset and factors are y and x.


Factor_Offset<- print.output %>% 
  str_subset("scale_factor|add_offset") #looking for scale factor or add_offset
LengthFactor_Offset=length(Factor_Offset)


#### Y factor and offset ----
y.scale_factor = Factor_Offset[LengthFactor_Offset-3] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
y.offset = Factor_Offset[LengthFactor_Offset-2] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()

#### X factor and offset ----
x.scale_factor = Factor_Offset[LengthFactor_Offset-1] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
x.offset = Factor_Offset[LengthFactor_Offset] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()


#### Getting Longitude and Latiude
#Follow the equations in Volume 3: Level1b products section 5.1.2.8
#Strange_Num <- ncvar_get(NC_file, "goes_imager_projection")

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

r.eq <- print.output %>% 
  str_subset("semi_major_axis") %>% 
  str_extract(number_pattern) %>% 
  as.numeric()

r.pol <- print.output %>% 
  str_subset("semi_minor_axis") %>% 
  str_extract(number_pattern) %>% 
  as.numeric()

e.value <- sqrt((r.eq^2-r.pol^2)/r.eq^2)

PPH <- print.output %>% #perspective point height
  str_subset("perspective_point_height") %>% 
  str_extract(number_pattern) %>% 
  as.numeric()

H <-  PPH + r.eq

lambda.o <- print.output %>% 
  str_subset("longitude_of_projection_origin") %>% 
  str_extract(number_pattern) %>% 
  as.numeric() %>% #It is in degrees
  deg2rad()
  


sin.sq <- function(num){
  (sin(num))^2
}
cos.sq <- function(num){
  (cos(num))^2
}
#lambda.o = -1.308996939 From manual value
YX.to.coords <- function(y,x){
  
  a = sin.sq(y) %>% 
    multiply_by((r.eq/r.pol)^2) %>% 
    add(cos.sq(y)) %>% 
    multiply_by(cos.sq(x)) %>% 
    add(sin.sq(x))
  
  b = H %>% 
    multiply_by(-2) %>% 
    multiply_by(cos(x)) %>% 
    multiply_by(cos(y))
  
  c = H %>% 
    raise_to_power(2) %>% 
    subtract(r.eq^2)
  
  r.s = b^2 %>% 
    subtract(4*a*c) %>% 
    raise_to_power(1/2) %>% 
    multiply_by(-1) %>% 
    subtract(b) %>% 
    divide_by(2*a)
  
  s.x = r.s %>% 
    multiply_by(cos(x)) %>% 
    multiply_by(cos(y))
  
  s.y = -r.s %>% 
    multiply_by(sin(x))
  
  s.z = r.s %>% 
    multiply_by(cos(x)) %>% 
    multiply_by(sin(y))
  
  lat = H %>% 
    subtract(s.x) %>% 
    raise_to_power(2) %>% 
    add(s.y^2) %>% 
    raise_to_power(-1/2) %>% 
    multiply_by(s.z) %>% 
    multiply_by((r.eq/r.pol)^2) %>% 
    atan()
  
  long = H %>% 
    subtract(s.x) %>% 
    raise_to_power(-1) %>% 
    multiply_by(s.y) %>% 
    atan() %>% 
    multiply_by(-1) %>% 
    add(lambda.o)
  
c(lat,long)
}

YX.to.coords(0.095340, -0.024052)

coords.to.YX <- function(lat, long){
  #Lat and long in GRS80
  phi.c = lat %>%
    tan() %>% 
    multiply_by((r.pol/r.eq)^2) %>% 
    atan()
  
  r.c = phi.c %>% 
    cos.sq() %>% 
    multiply_by(e.value^2) %>% 
    multiply_by(-1) %>% 
    add(1) %>% 
    raise_to_power(-1/2) %>% 
    multiply_by(r.pol)
  
  s.x = long %>% 
    subtract(lambda.o) %>% 
    cos() %>% 
    multiply_by(cos(phi.c)) %>% 
    multiply_by(-r.c) %>% 
    add(H)
  
  s.y = long %>% 
    subtract(lambda.o) %>% 
    sin() %>% 
    multiply_by(cos(phi.c)) %>% 
    multiply_by(-r.c)
  
  s.z = phi.c %>% 
    sin() %>% 
    multiply_by(r.c)
  
  y = atan(s.z/s.x)
  
  x = s.x^2 %>% 
    add(s.y^2) %>% 
    add(s.z^2) %>% 
    raise_to_power(-1/2) %>% 
    multiply_by(-s.y) %>% 
    asin()
  
  c(y,x)
}

coords.to.YX(0.590726966, -1.47813561)

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
