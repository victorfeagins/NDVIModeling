#!/bin/bash -l
#$ -N ModelingMissing #Name of job

#$ -P dietzelab # Set SCC project

module load R/4.0.5

Rscript /projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Modeling/Missing_Model_JobArray.R $SGE_TASK_ID