#Test Script
# Index to coordinates function

## Packages ----
library(ncdf4)
library(ncdf4.helpers)
library(stringr)
library(magrittr)


## Utility functions ----
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

sin.sq <- function(num){
  (sin(num))^2
}
cos.sq <- function(num){
  (cos(num))^2
}


File_info <- function(NC_file){
  number_pattern <- "-?[\\d]+.?[\\d]*e?-?[\\d]+"
  
  ### Printing_output ----
  print.output<- capture.output(NC_file)
  
  ### Coordinate Information -----
  Factor_Offset<- print.output %>% 
    str_subset("scale_factor|add_offset") #looking for scale factor or add_offset
  LengthFactor_Offset=length(Factor_Offset)
  
  #### y scale_factor and y offset -----
  y.scale_factor = Factor_Offset[LengthFactor_Offset-3] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  y.offset = Factor_Offset[LengthFactor_Offset-2] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  
  #### x scale_factor and x offset -----
  
  x.scale_factor = Factor_Offset[LengthFactor_Offset-1] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  x.offset = Factor_Offset[LengthFactor_Offset] %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  
  #### semi_major_axis  ----
  
  r.eq <- print.output %>% 
    str_subset("semi_major_axis") %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  
  #### semi_minor_axis  ----
  
  r.pol <- print.output %>% 
    str_subset("semi_minor_axis") %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  
  #### 1st eccentricity -----
  
  e.value <- sqrt((r.eq^2-r.pol^2)/r.eq^2)
  
  
  #### perspective point height -----
  
  PPH <- print.output %>% #perspective point height
    str_subset("perspective_point_height") %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  
  #### Satellite height from center of earth (m) -----
  
  H <-  PPH + r.eq
  
  
  #### longitude_of_projection_origin (rad) -----
  lambda.o <- print.output %>% 
    str_subset("longitude_of_projection_origin") %>% 
    str_extract(number_pattern) %>% 
    as.numeric() %>% #It is in degrees
    deg2rad() #converting to radians
  
  list(y.scale_factor = y.scale_factor, y.offset = y.offset,
       x.scale_factor = x.scale_factor, x.offset = x.offset,
       r.eq = r.eq,
       r.pol = r.pol,
       e.value = e.value,
       PPH = PPH,
       H = H,
       lambda.o = lambda.o)
}


filename = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc"
NC_file <- nc_open(filename)

NC_info <- File_info(NC_file)


angle.to.coords <- function(y,x, NC_infolist){
  
  a = sin.sq(y) %>% 
    multiply_by((NC_infolist$r.eq/NC_infolist$r.pol)^2) %>% 
    add(cos.sq(y)) %>% 
    multiply_by(cos.sq(x)) %>% 
    add(sin.sq(x))
  
  b = NC_infolist$H %>% 
    multiply_by(-2) %>% 
    multiply_by(cos(x)) %>% 
    multiply_by(cos(y))
  
  c = NC_infolist$H %>% 
    raise_to_power(2) %>% 
    subtract(NC_infolist$r.eq^2)
  
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
  
  lat = NC_infolist$H %>% 
    subtract(s.x) %>% 
    raise_to_power(2) %>% 
    add(s.y^2) %>% 
    raise_to_power(-1/2) %>% 
    multiply_by(s.z) %>% 
    multiply_by((NC_infolist$r.eq/NC_infolist$r.pol)^2) %>% 
    atan()
  
  long = NC_infolist$H %>% 
    subtract(s.x) %>% 
    raise_to_power(-1) %>% 
    multiply_by(s.y) %>% 
    atan() %>% 
    multiply_by(-1) %>% 
    add(NC_infolist$lambda.o)
  
  c(lat,long)
}

Index_to_angle <- function(y.index, x.index, NC_infolist){
  y.angle<- y.index %>% 
    subtract(NC_infolist$y.offset) %>% 
    divide_by(NC_infolist$y.scale_factor) %>% 
    multiply_by(1e-6)
  
  x.angle <- x.index %>% 
    subtract(NC_infolist$x.offset) %>% 
    divide_by(NC_infolist$x.scale_factor) %>% 
    multiply_by(1e-6)
  
  list(y.angle = y.angle, x.angle = x.angle)
}

Index_to_angle(558,1539, NC_info)

angle.to.coords(9039.5*1e-6, 5359.5*1e-6, NC_info)


getABI_Index <- function(lat,long,orbitVersion=NEW){
  #Function to determine the index of the ABI fixed grid that corresponds to a geodetic (normal) latitude and longitude.
  #lat and long should be in radians not degrees
  #Note: to index into the ABI fixed grid, you index by [y,x] not [x,y].
  #From PUG_L1b-vol3 pg 23
  
  #values from netcdf metadata:
  H <- 42164160  #goes_imagery_projection:perspective_point_height + goes_imagery_proj:semi-major_axis
  if(orbitVersion=="OLD"){
    long0 <- -1.562069680 #goes_imagery_projection:longitude_of_projection_origin
  }
  else{
    long0 <- -1.308996939 #goes_imagery_projection:longitude_of_projection_origin
  }
  r.pol <- 6356752.31414     #goes_imagery_projection:semi_minor_axis
  r.eq <- 6378137 #goes_imagery_projection:semi_major_axis
  e.value <- 0.0818191910435
  lat.c <- atan(r.pol**2/r.eq**2*tan(lat)) #geocentric latitude
  rc <- r.pol/(sqrt(1-e.value**2*(cos(lat.c))**2)) #geocentric distance to the point on the ellipsoid
  Sx <- H-rc*cos(lat.c)*cos(long-long0)
  Sy <- -rc*cos(lat.c)*sin(long-long0)
  Sz <- rc*sin(lat.c)
  y <- atan(Sz/Sx) #N/S Elevation Angle
  x <- asin(-Sy/(sqrt(Sx**2+Sy**2+Sz**2))) #E/W Scanning Angle
  return(c(y,x))
}
testlat = deg2rad(29.425171)
testlong = deg2rad(-98.494614)

getABI_Index(testlat, testlong, "OLD")

