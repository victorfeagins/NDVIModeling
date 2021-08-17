#!/bin/bash -l

#$ -N ExtractDataNCFile #Name of job
#$ -pe omp 4 #Grab 4 cores

# Set SCC project
#$ -P dietzelab

module load R/4.0.5
#Where the Extract_Data_Pipeline.R file is located
Rscript /projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Cron/Extract_Data_Pipeline.R $NSLOTS