#!/bin/bash
#R --no-save "--args $MYGROUP/WaSSI_AU/ $MYGROUP/WaSSI_AU/Data_AU_WaSSI parameters.txt 2000 2013 " <WaSSI_INPUT1.R> r-$SLURM_JOBID-$ALPS_APP_PE.log
R --no-save "--args /group/director1234/nliu/AU/DATA_PRE_VEG/ 82 94 0 " <GIMMS_NDVI_day2month.R> r-$SLURM_JOBID-$ALPS_APP_PE.log
