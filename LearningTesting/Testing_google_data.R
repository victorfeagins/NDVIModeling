library(ncdf4)
library(ncdf4.helpers)
library(stringr)
library(magrittr)
library(dplyr)
library(purrr)

### Functions -----
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

### Google Data ----

DataDirectoryG = "Test_Google_Data"

filesG = list.files(path=DataDirectoryG, full.names = TRUE, recursive=FALSE)

Channel2filesG <- str_subset(filesG, "L1b-RadC-M3C02_G16")

#Channel3files <- str_subset(files, "L1b-RadC-M3C03_G16")

#CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC-M3_G16")


NC_fileG<- nc_open(Channel2filesG)

NC_fileinfoG <- File_info(NC_fileG)





### Regular Data -----

DataDirectory = "Data/"

files = list.files(path=DataDirectory, full.names = TRUE, recursive=FALSE)

Channel2files <- str_subset(files, "L1b-RadC-M3C02_G16")

NC_file <-  nc_open(Channel2files[1])
NC_fileinfo <-  File_info(NC_file)

### Comparing ----
testlat = deg2rad(29.425171)
testlong = deg2rad(-98.494614)

Extract_Variable(testlat, testlong, NC_file, NC_fileinfo) %>% data.frame()

Extract_Variable(testlat, testlong, NC_fileG, NC_fileinfoG) %>% data.frame()

