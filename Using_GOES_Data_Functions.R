source("GOES_Data_Functions.R")


#Input ----
Datadirectory = "/projectnb/dietzelab/GOES_DataFTP/GOES_Data_2020/"


Latiude = c(32.457,  47.514,  45.806,  45.560,  44.065) #Can be a vector, in degrees 
Longitude = c(-91.9743, -93.469, -90.079, -84.714, -71.288) #Can be a vector, in degrees
# Russel Sage, Marcell, Willow Creek, UMBS, Bartlett

Dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-03"), by="days")



numcores = 4 #For sequential put 1


ptm <- proc.time()
plan(multisession, workers = numcores)

df = Extract_Dataframe_P(Datadirectory, Latiude, Longitude, Dates, average = TRUE)

(Time<- proc.time() - ptm)

# file = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330232189_e20172330234562_c20172330234599.nc"
# Open_Extract_Value(file, Latiude, Longitude, TRUE)
# 
# file = "Data/OR_ABI-L1b-RadC-M4C03_G16_s20172330140227_e20172330145029_c20172330145056.nc"
# Open_Extract_Value(file, Latiude, Longitude, TRUE)
# 
# 
# file = "Data/OR_ABI-L2-ACMC-M4_G16_s20172330150227_e20172330150227_c20172330156258.nc"
# Open_Extract_Value(file, Latiude, Longitude, TRUE)

# 
# NC_file <- nc_open(file)
# NC_info <- File_info(NC_file)
# FileRow<- Extract_Variable(Latiude,Longitude,NC_file,NC_info, TRUE) %>% 
#   data.frame()
# nc_close(NC_file)

write.csv(df, "TestDataAvg.csv")



