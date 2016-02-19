#!/bin/bash
##-----
## this file is used for submit rscript in magnus 
## --args are parameters for Rscript
## details of the runing can be found in r-$SLURM_JOBID-$ALPS_APP_PE.log file

## this is for producing WaSSI database of AU
#R --no-save "--args $MYGROUP/WaSSI_AU/ $MYGROUP/WaSSI_AU/Data_AU_WaSSI parameters.txt 2000 2013 " <WaSSI_INPUT1.R> r-$SLURM_JOBID-$ALPS_APP_PE.log

## this is for transfering 16days GIMMS_NDVI to month
#R --no-save "--args /group/director1234/nliu/AU/DATA_PRE_VEG/ 82 94 0 " <GIMMS_NDVI_day2month.R> r-$SLURM_JOBID-$ALPS_APP_PE.log

## this is for analysing PRE ~ VEG in AU
R --no-save "--args /group/director1234/nliu/AU/DATA_PRE_VEG/ " <AU_VEG_PRE.R> r-$SLURM_JOBID-$ALPS_APP_PE.log