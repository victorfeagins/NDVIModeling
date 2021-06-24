source("GOES_Data_Functions.R")


#Input ----
Datadirectory = "Data"
Latiude = 32.457 #Can be a vector, in degrees 
Longitude = -91.9743 #Can be a vector, in degrees
numcores = 4 #For sequential put 1



plan(multisession, workers = numcores)

df = Extract_Dataframe_P(Datadirectory, Latiude, Longitude, average = TRUE)



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

