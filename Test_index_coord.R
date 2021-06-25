# Index to coordinates function
# Coord to index functions
source("GOES_Data_Functions.R")



Latitude = 32.457 
Longitude = -91.9743 



# Channel 2 ----
file = "Data/OR_ABI-L1b-RadC-M3C02_G16_s20172330202189_e20172330204562_c20172330205000.nc"

NC_File <- nc_open(file)
NC_info <- File_info(NC_File)


coords.to.angle(Latitude, Longitude, NC_info)

(Index =coords.to.index(Latitude, Longitude, NC_info))


index.to.coord(Index$y.index, Index$x.index, NC_info)

index.to.angle(Index$y.index, Index$x.index, NC_info)

nc_close(NC_File)

# Channel 3 ----
file = "Data/OR_ABI-L1b-RadC-M3C03_G16_s20172330202189_e20172330204562_c20172330205008.nc"

NC_File <- nc_open(file)
NC_info <- File_info(NC_File)


coords.to.angle(Latitude, Longitude, NC_info)

(Index =coords.to.index(Latitude, Longitude, NC_info))


index.to.coord(Index$y.index, Index$x.index, NC_info)

index.to.angle(Index$y.index, Index$x.index, NC_info)
nc_close(NC_File)

# Cloud  ----
file = "Data/OR_ABI-L2-ACMC-M3_G16_s20172330937189_e20172330939562_c20172330940148.nc"

NC_File <- nc_open(file)
NC_info <- File_info(NC_File)


coords.to.angle(Latitude, Longitude, NC_info)

(Index =coords.to.index(Latitude, Longitude, NC_info))


index.to.coord(Index$y.index, Index$x.index, NC_info)

index.to.angle(Index$y.index, Index$x.index, NC_info)