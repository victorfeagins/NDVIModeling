#!/bin/bash -l


module load R/4.0.5

#Give count of how many files need to be modeled
num=$(Rscript /projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Modeling/Finding_Missing_Days.R)
Rscript /projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Modeling/Finding_Missing_Files.R # Runs missing file vector


/usr/local/bin/qsub -t 1-${num} /projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Modeling/RunningMissingModel.sh