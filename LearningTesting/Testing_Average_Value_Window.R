library(ncdf4)
library(ncdf4.helpers)
source("GOES_Data_Functions.R")

filename = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330227189_e20172330229562_c20172330229598.nc"

NC_file = nc_open(filename)

Latiude = 32.457 
Longitude = -91.9743 

NC_info = File_info(NC_file)

Indexinfo = coords.to.index(Latiude, Longitude, NC_info)


Rdata = ncvar_get(NC_file, "Rad")
Rdata[Indexinfo$y.index, Indexinfo$x.index]



nc.get.var.subset.by.axes(NC_file, "Rad", list(Y = Indexinfo$x.index , X = Indexinfo$y.index))


y.window = Indexinfo$y.index: (Indexinfo$y.index+1)

x.window = Indexinfo$x.index: (Indexinfo$x.index+1)


nc.get.var.subset.by.axes(NC_file, "Rad", list(Y = x.window, X = y.window))
