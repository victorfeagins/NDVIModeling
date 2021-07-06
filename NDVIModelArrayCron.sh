#!/bin/bash

Day=`date -d '-1 day' +'_%Y_%j_input'`

Num=`find /projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021 -type f -name "*${Day}*" | wc -l`


qsub -t 1-${Num} /projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/NDVIModelArray.sh