source("GOES_Data_Functions.R")


#Input ----
Datadirectory = "Data"
Latiude = 32.457 #Can be a vector, in degrees 
Longitude = -91.9743 #Can be a vector, in degrees
numcores = 4 #For sequential put 1



plan(multisession, workers = numcores)

df = Extract_Dataframe_P(Datadirectory, Latiude, Longitude)



