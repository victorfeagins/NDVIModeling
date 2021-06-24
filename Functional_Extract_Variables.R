# Functional Extracting Variables From NetCDF by coordinates -----------
# Raw Data -----
# Author: Victor Feagins
# Description: Extracting from .nc files variables I need to do analysis in a functional manner

#Input ----

#Datadirectory = "Data/" #Folder where GOES data is
Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/"
Latiude = 32.457 #Can be a vector, in degrees 
Longitude = -91.9743 #Can be a vector, in degrees
numcores = 9 #For sequential put 1

#Output ----

outputfilepath = "BigData.csv" #Name of file and path where want to be saved



## Packages ----
library(ncdf4) #Used to open .nc files
library(ncdf4.helpers) #Used to extract variables by index 
library(stringr)# Used to manipulate strings
library(magrittr)# Used for pipe friendly operations
library(dplyr)# Used for manipulate dataframes
library(purrr)# Used for applying functions on vectors

#Parrallel ----
library(future)
library(furrr)


##Packages used for analysis ----
library(ggplot2)

## Functions ----
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

File_info <- function(NC_file){
  # See Table 5.1.2.8 in volume 3 LEVEL 1B PRODUCTS
  #Parameters Required to Navigate Data Points on ABI Fixed Grid
  
  
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
# ## File Info ----
# filename = "Data/OR_ABI-L1b-RadC-M3C03_G16_s20172330622189_e20172330624562_c20172330625004.nc"
# NC_file <- nc_open(filename)
# 
# NC_info <- File_info(NC_file)

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


# testlatvector = c(29.425171, 42.360081)
# testlongvector = c(-98.494614, -71.058884)
# Lat_LongDf = data.frame(cbind(Lat = testlatvector, Long = testlongvector))

# #Testing vectorized deg2rad ----
# for (i in 1:nrow(Lat_LongDf)){
#   print("Lat")
#   print(deg2rad(Lat_LongDf$Lat[i]))
#   print("Long")
#   print(deg2rad(Lat_LongDf$Long[i]))
# }
# deg2rad(Lat_LongDf$Lat)
# deg2rad(Lat_LongDf$Long)
# #Is is vectorized
# 
# #Testing Vector Coords.to.angle ----
# for (i in 1:nrow(Lat_LongDf)){
#   print(coords.to.angle(Lat_LongDf$Lat[i], Lat_LongDf$Long[i], NC_info))
# }

# coords.to.angle(Lat_LongDf$Lat, Lat_LongDf$Long, NC_info)
# 

#It is vectorized

# 
# # Testing vector Coords.to.index ----
# for (i in 1:nrow(Lat_LongDf)){
#   print(coords.to.index(Lat_LongDf$Lat[i], Lat_LongDf$Long[i], NC_info))
# }
# 
# coords.to.index(Lat_LongDf$Lat, Lat_LongDf$Long, NC_info)
# 
# #It is vectorized

Extract_Variable <- function(lat, long, NC_file, NC_infolist){
  #Telling the Channel will rename the variables in the output.
  
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
    for (i in 1:length(lat)){
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

# 
# #Testing Extract_Variables ----
# Extract_Variable(Lat_LongDf$Lat, Lat_LongDf$Long, NC_file, NC_info)
# Extract_Variable(Lat_LongDf$Lat[1], Lat_LongDf$Long[2],NC_file, NC_info)
# 
# test <- nc_open("Data/OR_ABI-L2-ACMC-M4_G16_s20172330155227_e20172330155227_c20172330201262.nc")
# NC_info_cloud <- File_info(test)
# Extract_Variable(Lat_LongDf$Lat, Lat_LongDf$Long, test, NC_info_cloud)

Open_Extract_Value <- function(file, lat, long){
 NC_file <- nc_open(file)
 NC_info <- File_info(NC_file)
 FileRow<- Extract_Variable(lat,long,NC_file,NC_info) %>% 
   data.frame()
 nc_close(NC_file)
 return(FileRow)
}
# #Testing Open_Extract_Value ----
# Open_Extract_Value("Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc",
#                    Lat_LongDf$Lat,
#                    Lat_LongDf$Long)
# Open_Extract_Value("Data/OR_ABI-L1b-RadC-M3C03_G16_s20172330202189_e20172330204562_c20172330205008.nc",
#                    Lat_LongDf$Lat,
#                    Lat_LongDf$Long)
# 
# Open_Extract_Value("Data/OR_ABI-L2-ACMC-M4_G16_s20172330105227_e20172330105227_c20172330111256.nc",
#                    Lat_LongDf$Lat,
#                    Lat_LongDf$Long)

# Extract_Dataframe <- function(DataDirectory, lat, long){
#   #Eventually put in Time
#   files = list.files(path=DataDirectory, full.names = TRUE, recursive=FALSE)
#   
#   Channel2files <- str_subset(files, "L1b-RadC-M[\\d]C02_G16")
#   
#   Channel3files <- str_subset(files, "L1b-RadC-M[\\d]C03_G16")
#   
#   CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC")
#   
#   
#   DataCh2<- map_dfr(Channel2files, Open_Extract_Value, lat = lat, long = long)
#   
#   DataCh3<- map_dfr(Channel3files, Open_Extract_Value, lat = lat, long = long)
#   
#   DataCloud<- map_dfr(CloudMask, Open_Extract_Value, lat = lat, long = long)
#   
#   FinalData <- merge(DataCh2,DataCh3, by = c("Time", "Latitude", "Longitude"), all = TRUE) %>%
#   merge(DataCloud, by = c("Time", "Latitude", "Longitude"),all = TRUE)
#   
#   return(FinalData)
# }

# files = list.files(path="Data/", full.names = TRUE, recursive=FALSE)
# 
# files %>% 
#   str_subset("L1b-RadC-M[\\d]C03_G16") %>% 
#   length()
  

# ptm <- proc.time()
# 
# SeqData<- Extract_Dataframe("/projectnb/dietzelab/GOES_DataFTP/", 32.457, -91.9743)
# 
# proc.time() - ptm
# 
# Missing<- SeqData %>%
#   filter_all(any_vars(is.na(.))) #For some reason time of a different day are in there.


Extract_Dataframe_P <- function(DataDirectory, lat, long){
  #Eventually put in Time day as variable
  files = list.files(path=DataDirectory, full.names = TRUE, recursive=FALSE)
  
  Channel2files <- str_subset(files, "L1b-RadC-M[\\d]C02_G16")
  
  Channel3files <- str_subset(files, "L1b-RadC-M[\\d]C03_G16")
  
  CloudMask <-  str_subset(files, "OR_ABI-L2-ACMC")
  
  
  DataCh2<- future_map_dfr(Channel2files, Open_Extract_Value, lat = lat, long = long)
  
  DataCh3<- future_map_dfr(Channel3files, Open_Extract_Value, lat = lat, long = long)
  
  DataCloud<- future_map_dfr(CloudMask, Open_Extract_Value, lat = lat, long = long)
  
  FinalData <- merge(DataCh2,DataCh3, by = c("Time", "Latitude", "Longitude"), all = TRUE) %>%
    merge(DataCloud, by = c("Time", "Latitude", "Longitude"),all = TRUE)
  
  return(FinalData)
}

### Using the functions  ----

plan(multisession, workers = numcores)
#plan(sequential)

ptm <- proc.time()

ParData<- Extract_Dataframe_P(Datadirectory, Latiude, Longitude)

proc.time() - ptm


write.csv(ParData, outputfilepath)

# ParData %>%
# filter_all(any_vars(is.na(.))) #For some reason time of a different day are in there.



