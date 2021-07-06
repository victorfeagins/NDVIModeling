#!/bin/bash

#$ -N NDVIModelArray #Name of job

#$ -P dietzelab # Set SCC project

module load R/4.0.5

Rscript /projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/Run_Model_JobArray.R $SGE_TASK_ID