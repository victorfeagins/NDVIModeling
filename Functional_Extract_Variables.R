# Functional Extracting Variables From NetCDF by coordinates -----------
# Author: Victor Feagins
# Description: Extracting from .nc files variables I need to do analysis in a functional manner

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
coords.to.YX <- function(lat, long, NC_infolist){
  #Lat and long in GRS80

  phi.c = lat %>%
    tan() %>% 
    multiply_by((NC_infolist$r.pol/NC_infolist$r.eq)^2) %>% 
    atan()
  
  r.c = phi.c %>% 
    cos.sq() %>% 
    multiply_by(NC_infolist$e.value^2) %>% 
    multiply_by(-1) %>% 
    add(1) %>% 
    raise_to_power(-1/2) %>% 
    multiply_by(NC_infolist$r.pol)
  
  s.x = long %>% 
    subtract(NC_infolist$lambda.o) %>% 
    cos() %>% 
    multiply_by(cos(phi.c)) %>% 
    multiply_by(-r.c) %>% 
    add(NC_infolist$H)
  
  s.y = long %>% 
    subtract(NC_infolist$lambda.o) %>% 
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
  
  list(y.rad = y, x.rad = x)
}
## File Info ----



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

Coord.to.index <- function(lat,long, NC_infolist){
  #Coords have to be in GRS80
  
  RawCoord<- coords.to.YX(lat,long, NC_infolist)

y.index <- RawCoord$y.rad %>%
  multiply_by(NC_infolist$y.scale_factor) %>%
  add(NC_infolist$y.offset) %>%
  multiply_by(1e+6) %>% 
  round()

x.index <- RawCoord$x.rad %>%
  multiply_by(NC_infolist$x.scale_factor) %>%
  add(NC_infolist$x.offset) %>%
  multiply_by(1e+6) %>% 
  round()

  list(y.index = y.index, x.index = x.index)
}


### Kathryn Function -----
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

coords.to.YX(testlat, testlong, NC_info) # My functions works

getABI_Index(testlat,testlong, "OLD") #Function works outputs yx angle

Coord.to.index(testlat, testlong, NC_info)


getDataIndex <- function(vals,ch,orbitVersion=NEW){
  if(orbitVersion=="OLD"){
    if(ch==3){
      x.scale_factor <- 2.80000003840541e-05 #values from netcdf files under dimensions:x:scale_factor
      x.add_offset <- -0.0750259980559349 #All add_offset values changed when they moved the satellite
      y.scale_factor <- -2.80000003840541e-05
      y.add_offset <- 0.126545995473862
    }
    if(ch==2){
      x.scale_factor <- 1.40000001920271e-05
      x.add_offset <- -0.075033001601696
      y.scale_factor <- -1.40000001920271e-05
      y.add_offset <- 0.126552999019623
    }
    if(ch=="ACM"){
      x.scale_factor <- 5.60000007681083e-05
      x.add_offset <- -0.0750119984149933
      y.scale_factor <- -5.60000007681083e-05
      y.add_offset <- 0.126532003283501
      
    }
  }
  else{
    if(ch==3){
      x.scale_factor <- 2.80000003840541e-05 #values from netcdf files under dimensions:x:scale_factor
      x.add_offset <- -0.101346001029015
      y.scale_factor <- -2.80000003840541e-05
      y.add_offset <- 0.12822599709034
    }
    if(ch==2){
      x.scale_factor <- 1.40000001920271e-05
      x.add_offset <- -0.101352997124195
      y.scale_factor <- -1.40000001920271e-05
      y.add_offset <- 0.128233000636101
    }
    if(ch=="ACM"){
      x.scale_factor <- 5.60000007681083e-05
      x.add_offset <- -0.101332001388073
      y.scale_factor <- -5.60000007681083e-05
      y.add_offset <- 0.128212004899979
    }
  }
  i <- (vals[2]-x.add_offset)/x.scale_factor
  j <- (vals[1]-y.add_offset)/y.scale_factor
  return(c(i,j))
}

