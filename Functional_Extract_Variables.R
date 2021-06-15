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
coords.to.angle <- function(lat, long, NC_infolist){
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

coords.to.index <- function(lat,long, NC_infolist){
  #Coords have to be in GRS80 and in radians
  
  RawCoord<- coords.to.angle(lat,long, NC_infolist)

y.index <- RawCoord$y.rad %>%
  subtract(NC_infolist$y.offset) %>% 
  divide_by(NC_infolist$y.scale_factor) %>% 
  round()

x.index <- RawCoord$x.rad %>%
  subtract(NC_infolist$x.offset) %>% 
  divide_by(NC_infolist$x.scale_factor) %>% 
  round()

  list(y.index = y.index, x.index = x.index)
}



testlat = deg2rad(29.425171)
testlong = deg2rad(-98.494614)

test <- coords.to.angle(testlat, testlong, NC_info) # My functions works


coords.to.index(testlat, testlong, NC_info)


# Raw Data -----
#nc.get.var.subset.by.axes This one seems to the most useful.

Extract_Radiances <- function(lat, long, NC_file, NC_infolist){
  
  index <- coords.to.index(lat, long, NC_infolist)
  
  Value <- nc.get.var.subset.by.axes(NC_file, "Rad", list(Y=index$y.index, X=index$x.index)) %>% 
    multiply_by(NC_file$var$Rad$scaleFact) %>% 
    add(NC_file$var$Rad$addOffset)
  
  DataFlag <- nc.get.var.subset.by.axes(NC_file, "DQF", list(Y=index$y.index, X=index$x.index))
  Kappa <-  ncvar_get(NC_file,"kappa0")
  
  Time <-  ncvar_get(NC_file,"time_bounds") %>% 
    as.POSIXct(origin = "2000-01-01 12:00:00", tz = "UTC")
  Begin.Scan <- Time[1]
  End.Scan <-  Time[2]
  
  Lat <- rad2deg(lat)
  Long <-  rad2deg(long)
  
  list(Latitude = Lat, Longitude = Long, Radiances = Value, Kappa = Kappa, Begin.Scan = Begin.Scan,End.Scan = End.Scan,
       DataFlag = DataFlag)
}

Time_Bounds <-  ncvar_get(NC_file,"time_bounds")

Extract_Radiances(testlat, testlong, NC_file, NC_info)

testlatvector <- c(29.578100, 42.360081) %>% 
  deg2rad()

testlongvector <- c(-98.590080, -71.058884) %>% 
  deg2rad()

coords.to.angle(testlatvector, testlongvector, NC_info) #coords.to.angle works

coords.to.index(testlatvector, testlongvector, NC_info)


Extract_Radiances(testlatvector, testlongvector,NC_file, NC_info)

test.dataframe <- data.frame(list(Lat=testlatvector, Long=testlongvector))

test <- mapply(FUN = Extract_Radiances, testlatvector, testlongvector, MoreArgs= list(NC_file = NC_file, NC_infolist = NC_info)) %>% 
  t() %>% 
  data.frame()
test
