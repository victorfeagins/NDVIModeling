#!/bin/bash -l

#$ -N NDVIModelArray #Name of job

#$ -P dietzelab # Set SCC project

module load R/4.0.5
#Where Run_Model_JobArray.R is located
Rscript /projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Cron/Run_Model_JobArray.R $SGE_TASK_ID