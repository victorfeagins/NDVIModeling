source("GOES_Data_Functions.R")



Latiude = 32.457 
Longitude = -91.9743 


# Channel 2 ----
file = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc"



NC_File <- nc_open(file)
NC_info <- File_info(NC_File)

coords.to.angle(Latiude, Longitude, NC_info)

Ch2.index<- coords.to.index(Latiude, Longitude, NC_info)


Lat.rad = deg2rad(Latiude)

Long.rad = deg2rad(Longitude)

getABI_Index(Lat.rad, Long.rad, "OLD") %>% 
  getDataIndex(2, "OLD")


## Subset index vs total index

R2 <- ncvar_get(NC_File, "Rad")
R2[Ch2.index$y.index, Ch2.index$x.index]


nc.get.var.subset.by.axes(NC_File, "Rad", list(X=Ch2.index$x.index, Y=Ch2.index$y.index)) #Old not correctway

nc.get.var.subset.by.axes(NC_File, "Rad", list(X=Ch2.index$y.index, Y=Ch2.index$x.index)) #Correct way



# Impent correct way in function


source("GOES_Data_Functions.R")

Extract_Variable(Latiude, Longitude, NC_File, NC_info)


Extract_Variable(Latiude, Longitude, NC_File, NC_info, average = TRUE)


#Compare with extract
x.window = Ch2.index$x.index:(Ch2.index$x.index+1)

mean(R2[Ch2.index$y.index[1]:(Ch2.index$y.index[1]+1), Ch2.index$x.index[1]:(Ch2.index$x.index[1]+1)])

#Using function to extract all the data
numcores = 4 #For sequential put 1

plan(multisession, workers = numcores)

Datadirectory= "Data"
df <-  Extract_Dataframe_P(Datadirectory, Latiude, Longitude, average = TRUE)





