#!/bin/bash -l

Day=`date -d '-1 day' +'_%Y_%j_input'`

#Counts number of files that need to be modeled fron the inputfiles 
Num=`find /projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021 -type f -name "*${Day}*" | wc -l`

if [ ${Num} -gt 0 ] #there is a chance no days need to be modeled lack of sampling 
then
  /usr/local/bin/qsub -t 1-${Num} /projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/Cron/NDVIModelArray.sh
  
fi