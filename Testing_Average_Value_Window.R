library(ncdf4)
library(ncdf4.helpers)


filename = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330227189_e20172330229562_c20172330229598.nc"

NC_file = nc_open(filename)

nc.get.var.subset.by.axes(NC_file, "Rad", list(Y = 100, X = 200))


mean(nc.get.var.subset.by.axes(NC_file, "Rad", list(Y = 99:101, X = 199:201)))
