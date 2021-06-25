library(ncdf4) #Used to open .nc files
library(ncdf4.helpers) #Used to extract variables by index 
library(stringr)# Used to manipulate strings
library(magrittr)# Used for pipe friendly operations
library(dplyr)# Used for manipulate dataframes
library(purrr)# Used for applying functions on vectors

#Parrallel ----
library(future)
library(furrr)


rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}



File_info <- function(NC_file){
  # See Table 5.1.2.8 in volume 3 LEVEL 1B PRODUCTS
  #Parameters Required to Navigate Data Points on ABI Fixed Grid
  #Returns a list that other function will use for important variables
  number_pattern <- "-?[\\d]+.?[\\d]*e?-?[\\d]+" #Used as a regularexpression to extract numbers
  
  ### Printing_output ----
  #Many values are not outright extractable from the ncdf4 object
  #But are present when examining the ncdf4 output
  print.output<- capture.output(NC_file)
  
  ### Coordinate Information -----
  Factor_Offset<- print.output %>% 
    str_subset("scale_factor|add_offset") #looking for scale factor or add_offset
  LengthFactor_Offset=length(Factor_Offset)
  #assumes that the y.scalefactor & x.scalefactor are the last one in the file.
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
  #Look at table 5.1.2.8 for equation
  e.value <- sqrt((r.eq^2-r.pol^2)/r.eq^2)
  
  
  #### perspective point height -----
  
  PPH <- print.output %>% #perspective point height
    str_subset("perspective_point_height") %>% 
    str_extract(number_pattern) %>% 
    as.numeric()
  
  #### Satellite height from center of earth (m) -----
  #Look at table 5.1.2.8 for equation
  H <-  PPH + r.eq
  
  
  #### longitude_of_projection_origin (rad) -----
  #Converting to radians since all equation uses rads
  lambda.o <- print.output %>% 
    str_subset("longitude_of_projection_origin") %>% 
    str_extract(number_pattern) %>% 
    as.numeric() %>% #It is in degrees
    deg2rad() #converting to radians
  
  #### Time Info ----
  #Used as time info for the files.
  #Could also extract time infomation from ncdf4 object but sometimes it is missing
  Time = print.output %>% 
    str_subset("time_coverage_start") %>% 
    str_extract_all("[\\d]+[\\.]?[\\d]?") %>% 
    unlist() %>% 
    paste(collapse = "-") %>% 
    as.POSIXct(origin = "2000-01-01 12:00:00", format = "%Y-%m-%d-%H-%M-%S", tz = "UTC")
  
  
  list(y.scale_factor = y.scale_factor, y.offset = y.offset,
       x.scale_factor = x.scale_factor, x.offset = x.offset,
       r.eq = r.eq,
       r.pol = r.pol,
       e.value = e.value,
       PPH = PPH,
       H = H,
       lambda.o = lambda.o,
       Time = Time)
}


coords.to.angle <- function(lat, long, NC_infolist){
  #Lat and long in GRS80
  #See Section 5.1.2.8.2 in Product User Guide - L1b - Volume 3 for how to convert
  #Latitude and Longitdue to x and y viewing angles
  
  lat <- deg2rad(lat)
  long <- deg2rad(long)
  phi.c = lat %>%
    tan() %>% 
    multiply_by((NC_infolist$r.pol/NC_infolist$r.eq)^2) %>% 
    atan()
  
  r.c = phi.c %>% 
    cos() %>% 
    raise_to_power(2) %>% 
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


coords.to.index <- function(lat,long, NC_infolist){
  #Coords have to be in GRS80 and in radians
  #See section 5.1 of Product User Guide - Main - Volume 1 on how scale_factor
  #And add_offset should be applyed
  
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



index.to.angle <-  function(y.index,x.index, NC_infolist){
  y.angle <-  y.index %>% 
    multiply_by(NC_infolist$y.scale_factor) %>% 
    add(NC_infolist$y.offset)
  
  x.angle <- x.index %>% 
    multiply_by(NC_infolist$x.scale_factor) %>% 
    add(NC_infolist$x.offset)
  list(y.angle = y.angle, x.angle = x.angle)
}


index.to.coord <- function(y.index, x.index, NC_infolist){
  Angles = index.to.angle(y.index, x.index, NC_infolist)
  y = Angles$y.angle
  x = Angles$x.angle
  
  
  a <- y %>% 
    sin() %>% 
    raise_to_power(2) %>% 
    multiply_by((NC_infolist$r.eq/NC_infolist$r.pol)^2) %>% 
    add(cos(y)^2) %>% 
    multiply_by(cos(x)^2) %>% 
    add(sin(x)^2)
  
  b <- -2 %>% 
    multiply_by(NC_infolist$H) %>% 
    multiply_by(cos(x)) %>% 
    multiply_by(cos(y))
  
  c <-  NC_infolist$H %>% 
    raise_to_power(2) %>% 
    subtract(NC_infolist$r.eq^2)
  
  r.s <- a %>% 
    multiply_by(c) %>% 
    multiply_by(-4) %>% 
    add(b^2) %>% 
    raise_to_power(1/2) %>% 
    multiply_by(-1) %>% 
    add(-b) %>% 
    divide_by(2*a)
  
  s.x <- r.s %>% 
    multiply_by(cos(x)) %>% 
    multiply_by(cos(y))
  
  s.y <- -r.s %>% 
    multiply_by(sin(x))
  
  s.z <- r.s %>% 
    multiply_by(cos(x)) %>% 
    multiply_by(sin(y))
  
  
  Latitude <-  NC_infolist$H %>% 
    subtract(s.x) %>% 
    raise_to_power(2) %>% 
    add(s.y^2) %>% 
    raise_to_power(-1/2) %>% 
    multiply_by(s.z) %>% 
    multiply_by((NC_infolist$r.eq/NC_infolist$r.pol)^2) %>% 
    atan() %>% 
    rad2deg()
  
  
  Longitude <- s.y %>% 
    divide_by(NC_infolist$H - s.x) %>% 
    atan() %>% 
    multiply_by(-1) %>% 
    add(NC_infolist$lambda.o) %>% 
    rad2deg()
  
  
  
  return(list(Latitude = Latitude, Longitude = Longitude))
  
}


Extract_Variable <- function(lat, long, NC_file, NC_infolist, average = FALSE){
  #Opens files and extracts values important for NDVI calculations
  #Ideally this function could be more flexiable but for now it is fine
  #Returns a Dataframe
  
  
  index <- coords.to.index(lat, long, NC_infolist) #Grabs The index 
  
  filename <-  NC_file$filename 
  
  if(str_detect(filename, "OR_ABI-L2-ACMC")){
    Varname <-  "BCM"
    Outputname <-  "BCM"
  } else if (str_detect(filename, "L1b-RadC-M[\\d]C02_G16")){
    Varname <- "Rad"
    Outputname <- "RadC02"
    
  } else if (str_detect(filename, "L1b-RadC-M[\\d]C03_G16")){
    Varname <- "Rad"
    Outputname <- "RadC03"
  }
  Value = vector(mode = "numeric", length(lat))
  DataFlag = vector(mode = "numeric", length(lat))
  
  ### Applying Scale offset ----
  if (average == FALSE | Outputname != "RadC02" ){
    
 
  for (i in 1:length(lat)){
    #For every lat and longitude take the value of the index
    if (NC_file$var[[Varname]]$hasScaleFact){
      Value[i] <- nc.get.var.subset.by.axes(NC_file, Varname, list(Y=index$y.index[i], X=index$x.index[i]))
    } else {
      Value[i] <- nc.get.var.subset.by.axes(NC_file, Varname, list(Y=index$y.index[i], X=index$x.index[i]))
      
    }
    
    DataFlag[i] <- nc.get.var.subset.by.axes(NC_file, "DQF", list(Y=index$y.index[i], X=index$x.index[i]))
    if (Varname == "Rad"){
      Kappa <-  ncvar_get(NC_file,"kappa0")
      Offset <- NC_file$var[[Varname]]$addOffset
      ScaleFact <- NC_file$var[[Varname]]$scaleFact 
    }
  }}
  
  if(average == TRUE){
    for (i in 1:length(lat)){
    # y.window =  (index$y.index[i]-1):(index$y.index[i]+1)
    # x.window = (index$x.index[i]-1):(index$x.index[i]+1)
    y.window =  (index$y.index[i]):(index$y.index[i]+1)
    x.window = (index$x.index[i]):(index$x.index[i]+1)
    
    
    if (NC_file$var[[Varname]]$hasScaleFact){
      Value[i] <- mean(nc.get.var.subset.by.axes(NC_file, Varname, list(Y=y.window, X=x.window)))
    } else {
      Value[i] <- mean(nc.get.var.subset.by.axes(NC_file, Varname, list(Y=y.window, X=x.window)))
      
    }
    
    DataFlag[i] <- mean(nc.get.var.subset.by.axes(NC_file, "DQF", list(Y=y.window, X=x.window)))
    }
    if (Varname == "Rad"){
      Kappa <-  ncvar_get(NC_file,"kappa0")
      Offset <- NC_file$var[[Varname]]$addOffset
      ScaleFact <- NC_file$var[[Varname]]$scaleFact 
    }
  }
  
  
  #End of scalefactor code----------------------------------------
  
  
  Time = NC_infolist$Time
  
  
  Lat <- lat
  Long <-  long
  
  
  if (Outputname == "RadC02"){
    #Going to include Kappa
    return(list(Latitude = Lat, Longitude = Long, 
                RadC02 = Value, KappaC02 = Kappa, 
                Time = Time,
                RadC02DQF = DataFlag,
                RadC02ScaleFactor = ScaleFact,
                RadC02Offset = Offset) %>% 
             data.frame())
  } else if (Outputname == "RadC03"){
    return(list(Latitude = Lat, Longitude = Long, 
                RadC03 = Value, KappaC03 = Kappa, 
                Time = Time,
                RadC03DQF = DataFlag,
                RadC03ScaleFactor = ScaleFact,
                RadC03Offset = Offset) %>% 
             data.frame())
    
  } else if(Outputname == "BCM"){
    return(list(Latitude = Lat, Longitude = Long, 
                BCM = Value,
                Time = Time,
                BCMDQF = DataFlag)) %>% 
      data.frame()}
}


Open_Extract_Value <- function(file, lat, long, average = FALSE){
  #Opens and closes the files after getting info
  #Lat and Long can be vectors
  #Returns dataframe of row(s) of data per lat and long
  NC_file <- nc_open(file)
  NC_info <- File_info(NC_file)
  FileRow<- Extract_Variable(lat,long,NC_file,NC_info, average) %>% 
    data.frame()
  nc_close(NC_file)
  return(FileRow)
}



Extract_Dataframe_P <- function(DataDirectory, lat, long, average = FALSE){
  #Eventually put in Time day as variable to filter files day
  #Uses all the functions to create large dataframe containing info on all the 
  #Files in the datadirectory
  #Can be uses in parrallel using the future framework and furr
  #Outside of this function set a future::plan
  
  files = list.files(path=DataDirectory, full.names = TRUE, recursive=FALSE)
  
  Channel2files <- str_subset(files, "L1b-RadC-M[\\d]C02_G16")
  
  Channel3files <- str_subset(files, "L1b-RadC-M[\\d]C03_G16")
  
  CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC")
  
  
  DataCh2<- future_map_dfr(Channel2files, Open_Extract_Value, lat = lat, long = long, average = average)
  
  DataCh3<- future_map_dfr(Channel3files, Open_Extract_Value, lat = lat, long = long, average = average)
  
  DataCloud<- future_map_dfr(CloudMask, Open_Extract_Value, lat = lat, long = long, average = average)
  
  FinalData <- merge(DataCh2,DataCh3, by = c("Time", "Latitude", "Longitude"), all = TRUE) %>%
    merge(DataCloud, by = c("Time", "Latitude", "Longitude"),all = TRUE)
  
  return(FinalData)
}



