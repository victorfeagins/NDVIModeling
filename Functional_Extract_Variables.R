
# Raw Data -----# Functional Extracting Variables From NetCDF by coordinates -----------
# Author: Victor Feagins
# Description: Extracting from .nc files variables I need to do analysis in a functional manner

## Packages ----
library(ncdf4)
library(ncdf4.helpers)
library(stringr)
library(magrittr)
library(dplyr)
library(purrr)

library(future)
library(furrr)
## Utility functions ----
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

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

sin.sq <- function(num){
  (sin(num))^2
}
cos.sq <- function(num){
  (cos(num))^2
}
coords.to.angle <- function(lat, long, NC_infolist){
  #Lat and long in GRS80
  lat <- deg2rad(lat)
  long <- deg2rad(long)
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
filename = "Data/OR_ABI-L1b-RadC-M3C03_G16_s20172330622189_e20172330624562_c20172330625004.nc"
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


testlatvector = c(29.425171, 42.360081)
testlongvector = c(-98.494614, -71.058884)
Lat_LongDf = data.frame(cbind(Lat = testlatvector, Long = testlongvector))

#Testing vectorized deg2rad ----
for (i in 1:nrow(Lat_LongDf)){
  print("Lat")
  print(deg2rad(Lat_LongDf$Lat[i]))
  print("Long")
  print(deg2rad(Lat_LongDf$Long[i]))
}
deg2rad(Lat_LongDf$Lat)
deg2rad(Lat_LongDf$Long)
#Is is vectorized

#Testing Vector Coords.to.angle ----
for (i in 1:nrow(Lat_LongDf)){
  print(coords.to.angle(Lat_LongDf$Lat[i], Lat_LongDf$Long[i], NC_info))
}

coords.to.angle(Lat_LongDf$Lat, Lat_LongDf$Long, NC_info)


#It is vectorized


# Testing vector Coords.to.index ----
for (i in 1:nrow(Lat_LongDf)){
  print(coords.to.index(Lat_LongDf$Lat[i], Lat_LongDf$Long[i], NC_info))
}
coords.to.index(-98.49461, 29.42517, NC_info)
coords.to.index(Lat_LongDf$Lat[1], Lat_LongDf$Long[1], NC_info)


coords.to.index(Lat_LongDf$Lat, Lat_LongDf$Long, NC_info)

#nc.get.var.subset.by.axes This one seems to the most useful.

Extract_Variable <- function(lat, long, NC_file, NC_infolist){
  #Telling the Channel will rename the variables in the output.
  
  index <- coords.to.index(lat, long, NC_infolist) #Grabs The index 
  
  filename <-  NC_file$filename 
  
  if(str_detect(filename, "OR_ABI-L2-ACMC-M3")){
    Varname <-  "BCM"
    Outputname <-  "BCM"
  } else if (str_detect(filename, "L1b-RadC-M3C02_G16")){
    Varname <- "Rad"
    Outputname <- "RadC02" 
    
  } else if (str_detect(filename, "L1b-RadC-M3C03_G16")){
    Varname <- "Rad"
    Outputname <- "RadC03"
  }
  
  if (NC_file$var[[Varname]]$hasScaleFact){
    Value <- nc.get.var.subset.by.axes(NC_file, Varname, list(Y=index$y.index, X=index$x.index)) %>% 
      multiply_by(NC_file$var[[Varname]]$scaleFact) %>% 
      add(NC_file$var[[Varname]]$addOffset)
  } else {
    Value <- nc.get.var.subset.by.axes(NC_file, Varname, list(Y=index$y.index, X=index$x.index))
  }

  
  DataFlag <- nc.get.var.subset.by.axes(NC_file, "DQF", list(Y=index$y.index, X=index$x.index))
  if (Varname == "Rad"){
    Kappa <-  ncvar_get(NC_file,"kappa0")
  }
  
  
  # Time <-  ncvar_get(NC_file,"time_bounds") %>%
  #   as.POSIXct(origin = "2000-01-01 12:00:00", tz = "UTC") %>% 
  #   round.POSIXt("secs")
  # Begin.Scan <- Time[1]
  # End.Scan <-  Time[2]
  
  Time <-  ncvar_get(NC_file,"t") %>%
    na_if(-999) %>% 
    as.POSIXct(origin = "2000-01-01 12:00:00", tz = "UTC") %>% 
    round.POSIXt("secs")
  
  Lat <- rad2deg(lat) %>% 
    round(5)
  Long <-  rad2deg(long) %>% 
    round(5)

  
  if (Outputname == "RadC02"){
    #Going to include Kappa
    return(list(Latitude = Lat, Longitude = Long, 
                RadC02 = Value, KappaC02 = Kappa, 
                Time = Time,
                RadC02DQF = DataFlag))
  } else if (Outputname == "RadC03"){
    return(list(Latitude = Lat, Longitude = Long, 
                RadC03 = Value, KappaC03 = Kappa, 
                Time = Time,
                RadC03DQF = DataFlag))
    
  } else if(Outputname == "BCM"){
    return(list(Latitude = Lat, Longitude = Long, 
                BCM = Value,
                Time = Time,
                BCMDQF = DataFlag))}
}

Time_Bounds <-  ncvar_get(NC_file,"time_bounds")


Extract_Variable(testlat, testlong,NC_file, NC_info) 




#### Temporally Extract the variables

DataDirectory = "Data/"

files = list.files(path=DataDirectory, full.names = TRUE, recursive=FALSE)

Channel2files <- str_subset(files, "L1b-RadC-M3C02_G16")

Channel3files <- str_subset(files, "L1b-RadC-M3C03_G16")

CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC-M3_G16")

NamePending <- function(file, lat, long){
 NC_file <- nc_open(file)
 NC_info <- File_info(NC_file)
 FileRow<- Extract_Variable(lat,long,NC_file,NC_info) %>% 
   data.frame()
 nc_close(NC_file)
 return(FileRow)
}

# NamePending(CloudMask[1], testlat,testlong)

 # ptm <- proc.time()
 # DataCh2<- map_dfr(Channel2files, NamePending, lat = testlat, long = testlong)
 # 
 # DataCh3<- map_dfr(Channel3files, NamePending, lat = testlat, long = testlong)
 # 
 # DataCloud<- map_dfr(CloudMask, NamePending, lat = testlat, long = testlong)

 # proc.time() - ptm
 # 
 # 
 # test<- merge(DataCh2,DataCh3, by = c("Time", "Latitude", "Longitude"), all = TRUE) %>%
 #   merge(DataCloud, by = c("Time", "Latitude", "Longitude"),all = TRUE)

# 
# plan(multisession)
# 
# ptm <- proc.time()
# Futured1<- future_map_dfr(Channel2files, NamePending, lat = testlat, long = testlong)
# 
# Futured2<- future_map_dfr(Channel3files, NamePending, lat = testlat, long = testlong)
# 
# Futured3<- future_map_dfr(CloudMask, NamePending, lat = testlat, long = testlong)
# proc.time() - ptm
# 
# test<- merge(Futured1,Futured2, by = c("Time", "Latitude", "Longitude"), all = TRUE) %>%
#   merge(Futured3, by = c("Time", "Latitude", "Longitude"),all = TRUE)
# 
Extract_FinalData <- function(DataDirectory, lat, long){
  #Eventually put in Time
  files = list.files(path=DataDirectory, full.names = TRUE, recursive=FALSE)
  
  Channel2files <- str_subset(files, "L1b-RadC-M3C02_G16")
  
  Channel3files <- str_subset(files, "L1b-RadC-M3C03_G16")
  
  CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC-M3_G16")
  
  
  DataCh2<- map_dfr(Channel2files, NamePending, lat = testlat, long = testlong)
  
  DataCh3<- map_dfr(Channel3files, NamePending, lat = testlat, long = testlong)
  
  DataCloud<- map_dfr(CloudMask, NamePending, lat = testlat, long = testlong)
  
  FinalData <- merge(DataCh2,DataCh3, by = c("Time", "Latitude", "Longitude"), all = TRUE) %>%
  merge(DataCloud, by = c("Time", "Latitude", "Longitude"),all = TRUE)
  
  return(FinalData)
}

ptm <- proc.time()

SeqData<- Extract_FinalData("Data/", testlat, testlong)

proc.time() - ptm
Extract_FinalDataP <- function(DataDirectory, lat, long){
  #Eventually put in Time
  plan(multisession)
  files = list.files(path=DataDirectory, full.names = TRUE, recursive=FALSE)
  
  Channel2files <- str_subset(files, "L1b-RadC-M3C02_G16")
  
  Channel3files <- str_subset(files, "L1b-RadC-M3C03_G16")
  
  CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC-M3_G16")
  
  
  DataCh2 <- future_map_dfr(Channel2files, NamePending, lat = testlat, long = testlong)
  
  DataCh3 <- future_map_dfr(Channel3files, NamePending, lat = testlat, long = testlong)
  
  DataCloud <- future_map_dfr(CloudMask, NamePending, lat = testlat, long = testlong)
  
  FinalData <- merge(DataCh2,DataCh3, by = c("Time", "Latitude", "Longitude"), all = TRUE) %>%
    merge(DataCloud, by = c("Time", "Latitude", "Longitude"),all = TRUE)
  
  return(FinalData)
}
ptm <- proc.time()
ParData<- Extract_FinalDataP("Data/", testlat, testlong)

proc.time() - ptm

#microbenchmark::microbenchmark(Extract_FinalDataP("Data/", testlat, testlong), Extract_FinalData("Data/", testlat, testlong), times = 5)
