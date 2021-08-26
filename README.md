# Automating Modeling of Normalized Difference Vegetation Index (NDVI)

## What This repo is for
This repo to for my advisers during the BRITE REU at Boston University to use and implement in their lab to automatically get daily estimates of NDVI values. Others may consult with and admire this repo if they would like to do something similar. 

## Essential Directions
To retrieve the NDVI values all essential scripts are located in the **Cron** folder

### Set Up
#### Data
Data utilized are the netcdf files from the National Oceanic and Atmospheric Administration's (NOAA) Geostationary Operational Environmental Satellite 16 (GOES 16) to download this files please consult with NOAA's comprehensive large array-data stewardship system (CLASS): https://www.avl.class.noaa.gov/saa/products/welcome

The data products required are the following under GOES-R Series ABI Products GRABIPRD (partially restricted L1b and L2+ Data Products) 
- Datatype: ABI L1b Radiances Data, ABI Channel: C02, ABI scan Sector: CONUS, Satellite: G16
- Datatype: ABI L1b Radiances Data, ABI Channel: C03, ABI scan Sector: CONUS, Satellite: G16
- ABI L2+ Product Data, Product Type Clear Sky Masks, ABI scan Sector CONUS, Satellite: G16

All this netcdf files should be downloaded into one directory as that is how the scripts are set up to handle the data.
Also don't rename the files as they contain important information about the time and data source.

In another repo I have the code to acquire NOAA data automatically from CLASS subscription emails. Since that process requires setting up an email account, security preferences, authorization it is set to private so only my advisers can see it. If you are struggling with CLASS email subscriptions and would like to learn how I handled the problem, feel free to reach out victorfeagins@att.net

#### Site CodeBook
The netcdf files are the entire CONUS area of the satellite. Usually we want to look at a single area. To filter the netcdf files by latiude and longtude we create a Site Codebook. Site Codebook has three variables
- siteName : The name of the location 
- Lat: Latiude in degrees of location
- Long: Longitude in degrees of location

Take a look at GOESdownloadSites.csv in the Cron folder for an example

#### Updating Scripts
Your work environment will be different then mine particular where your data is saved, where you want to save your models and save the NDVI daily estimates. The important locations that need be to define are the following:
- Directory that contains NetCDF files
- Directory that will contain the cleaned extracted data from the NetCDF files
- Directory that contain the MCMC objects from the modeling procedure
- Directory for the daily estimates of NDVI values. 

The only thing that needs to be changed scripts are the file paths to match your environment

## Execution 
### Automation
Automation was achieved using the job scheduler and cron program with Boston University's Shared Computing Cluster.
The Cron folder in this repo has all the scripts and .sh files needed to extract data from netcdf files and model with them. I used the cron program to automatically run the scripts daily on yesterday's data. I used qsub command to schedule the jobs and job arrays.

### Manual
To run scripts manually from the other folders not Cron.
RawDataProcessing : Extract_Data_Pipeline_Manual.R will allow you to extract raw data of dates range of netcdf files

Modeling process is not set up to run manually since it takes a long time to run and there is no guarantee that the model will converge. If you wanted to run it manually I recommend taking the `DiurnalModeling` function from Missing_Model_JobArray.R and running individually on the csv's from the Extract_Data_Pipeline_Manual.R process.

## Other Code
#### RawDataProcessing
This folder contains the ever imporant GOES_Data_functions.R. This script contains many functions that are important for converting latitude and longitude into index for the netcdf files and extracting important meta data from netcdf files. I translate many transformations contained in the various manuals for the GOES data into R functions.

#### Modeling
This folder contains the way to model for missing days that did not converge.

#### Shiny
I made a shiny app that visualizes the model and data from this process. It allows you to select a site location view their daily estimates and view each days individual model. 

#### LearningTesting
Basically archived code of me learning things and testing things. Nothing too important but sometimes I write neat code that I want to return to that may have not worked in this project but might work on another project.

#### Post Modeling
Me doing some quick checks to see how the files ran nothing too important in there.

## More Info 
If you would like learn more about the model and applications that motivated this work check out some of my advisers papers on the topic 
- A Statistical Model for Estimating Midday NDVI from the Geostationary Operational Environmental Satellite (GOES) 16 and 17: https://www.mdpi.com/2072-4292/11/21/2507
- Improving the monitoring of deciduous broadleaf phenology using the Geostationary Operational Environmental Satellite (GOES) 16 and 17 https://bg.copernicus.org/articles/18/1971/2021/bg-18-1971-2021-discussion.html

## Acknowledgements 
This work was funded, in part, by NSF grant DBI-1949968, awarded to the Boston University Bioinformatics BRITE REU program and The Near-term Ecological Forecasting Initiative NSF MSB Grant 1638577


