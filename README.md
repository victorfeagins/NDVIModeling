# Automating Modeling of Normalized Difference Vegetation Index (NDVI)

## What This repo is for
This repo to for my advisers during the BRITE REU at Boston University to use and implement in their lab to automatically get daily estimates of NDVI values. Others may consult with and admire this repo if they would like to do something similar. 

## Essential Directions
To retrive the NDVI values all essential scripts are located in the **Cron** folder

### Set Up
#### Data
Data utlized are the netcdf files from the National Oceanic and Atmospheric Administration's (NOAA) Geostationary Operational Environmental Satellite 16 (GOES 16) to download this files please consult with NOAA's comprehensive large array-data stewardship system (CLASS): https://www.avl.class.noaa.gov/saa/products/search?sub_id=0&datatype_family=GRABIPRD&submit.x=18&submit.y=3

The data products required are the following
- Datatype: ABI L1b Radiances Data, ABI Channel: C02, ABI scan Sector: CONUS, Satellite: G16
- Datatype: ABI L1b Radiances Data, ABI Channel: C03, ABI scan Sector: CONUS, Satellite: G16
- ABI L2+ Product Data, Product Type Clear Sky Masks, ABI scan Sector CONUS, Satellite: G16

All this datafiles should be downloaded into one directory.


WIP


## Acknowledgements 
All code created was during the Bioinformatics Research and Interdisciplinary Training Experience (BRITE) at Boston University with my advisers Kathyern Wheeler and Michael Dietze and the Near-term Ecological Forecasting Initiative
