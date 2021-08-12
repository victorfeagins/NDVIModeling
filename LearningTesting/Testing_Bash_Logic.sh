#!/bin/bash

Day=`date -d '-1 day' +'_%Y_%j_input'`

Num=`find /projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021 -type f -name "*${Day}*" | wc -l`


if [ ${Num} -gt 0 ]

then
  echo "You can see this"
fi