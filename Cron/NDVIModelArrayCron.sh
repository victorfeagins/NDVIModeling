#!/bin/bash -l

Day=`date -d '-1 day' +'_%Y_%j_input'`

Num=`find /projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021 -type f -name "*${Day}*" | wc -l`

if [ ${Num} -gt 0 ]
then
  /usr/local/bin/qsub -t 1-${Num} /projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/Cron/NDVIModelArray.sh
  
fi