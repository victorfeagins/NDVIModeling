#!/bin/bash -l

#$ -N ExtractDataNCFile #Name of job
#$ -pe omp 4 #Grab 4 cores

# Set SCC project
#$ -P dietzelab

module load R/4.0.5

Rscript /projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/Extract_Data_Pipeline.R $NSLOTS