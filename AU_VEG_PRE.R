##################################################

## this Rscript is used for analysing the relationship between PRE and VEG in AU
## the input data for this code includes an ENVI database for WaSSI model with a parameters.txt and other ENVI data like GIMMS NDVI and Soil moisture data
## the *.hdr file for WaSSI inlcudes: DEM; Monthly PRE, TEMP, LAI; MODIS land cover; 11 SOIL parameters developed for SMA-SMC 

##################################################

##!/bin/Rscript

print("Starting to process")

## requirement for this Rscript
	## read required parameters for R script
	## parameters are: [1] location of data; [2] file name of input ENVI data; [3] parameters file for ENVI data; [4] Year start for WaSSI; [5] Year End for WaSSI
	
args <- commandArgs(TRUE)

##load all required libraries

library(caTools)
library(plyr)
library(reshape2)
library(pryr)
library(parallel)
library(bfast)
library(trend)

## Load functions which are included in "Funcs_AU_PRE.R"

source("Funcs_AU_PRE.R")

## set the first args as the directory for processing --- the data location

setwd(args[1])

if (0){    	# this is used for commenting codes

## process GIMMS NDVI data  ---- Read original ENVI MMS monthly data which was recalculated with flag data from  original16days data base

if (! file.exists("NDVI_GIMMS_82_94_frame.RData")){

	print("read GIMMS NDVI from original ENVI Data")
	ENVI_GIMMS_NDVI<-read.ENVI("NDVI_AU_GIMMS_94_82_flag_5km")
	#Infos of the ENVI data can be read from the *.hdr
	NDVI_GIMMS_82_94_frame<-data.frame(ID=c(1:(dim(ENVI_GIMMS_NDVI)[1]*dim(ENVI_GIMMS_NDVI)[2])),YEAR=rep(c(1994:1982),each=12*dim(ENVI_GIMMS_NDVI)[1]*dim(ENVI_GIMMS_NDVI)[2]),MONTH=rep(rep(c(9,10,11,5,3,6,7,1,2,12,8,4),13),each=dim(ENVI_GIMMS_NDVI)[1]*dim(ENVI_GIMMS_NDVI)[2]),NDVI=as.vector(ENVI_GIMMS_NDVI))
	NDVI_GIMMS_82_94_frame<-arrange(NDVI_GIMMS_82_94_frame,ID,YEAR,MONTH)
	save(NDVI_GIMMS_82_94_frame,file="NDVI_GIMMS_82_94_frame.RData")
	
}

## Creat a folder for storaging the struction of all dataframes in R memory

	if (! dir.exists("infos")){dir.create("infos", showWarnings = TRUE, recursive = FALSE, mode = "0777")}

	## read all exist framedata stored in the driver in *.RData and output their infos to "infos/summary.txt"
	print('loading existing data bases')

	## get names for all exist *RData file in the directory
	filenames<-dir(path=".", include.dirs=TRUE, all.files =TRUE, pattern = "*.RData", full.names = TRUE)

	## read all .RData files to memory
	for (i in c(1:length(filenames))){
		
		load(filenames[i])
		print(mem_used())
	}
	
	list<-ls()
	print(list)

	## print summary for all objects in memory to a summary.txt
	info_file="infos/summary.txt"
	file.create(info_file)

	for (i in c(1:length(list))){
  		write(paste("object name=",list[i]),file=info_file,append=TRUE)
		write(paste("object dim= \n",dim(get(list[i]))),file=info_file,append=TRUE)
		write(summary(get(list[i])),file=info_file,append=TRUE)
		print(list[i])
		str(get(list[i]))
	}
}  ##------- this is used for commenting codes


if (0){  ##---------this is for commenting codes

## merge NDVI databases (NAHH 1995-2013 and GIMMS 1982-1994) and save data to "NDVI_mon_82_13.RData"

if (file.exists("NDVI_mon_82_13.RData")){
	
	print("load NDVI data")
	
	}else{
	
	names(NDVI_GIMMS_82_94_frame)[3]<-"Month"
	names(NDVI_GIMMS_82_94_frame)[4]<-"NDVI"
	names(NDVI_NAHH_mon_frame)[4]<-"NDVI"
	
	NDVI_mon_82_13<-rbind(NDVI_GIMMS_82_94_frame,NDVI_NAHH_mon_frame)
	NDVI_mon_82_13$NDVI[NDVI_mon_82_13$NDVI<0]<-NA
	
	save(NDVI_mon_82_13,file="NDVI_mon_82_13.RData")
	
}

##clear memory
rm(data_climate,NDVI_GIMMS_82_94_frame,NDVI_NAHH_mon_frame,NDVI_NAHH_ann_frame_95_13)
	
## Sort all monthly data by ID, YEAR, MONTH and annual data by ID, YEAR
climate_mon_frame_70_13<-arrange(climate_mon_frame_70_13,ID,YEAR,Month)
NDVI_mon_82_13<-arrange(NDVI_mon_82_13,ID,YEAR,Month)
LAI_mon_frame_82_13<-arrange(LAI_mon_frame_82_13,ID,YEAR,Month)
LAI_ann_frame_82_13<-arrange(LAI_ann_frame_82_13,ID,YEAR)
sm_awap_ann_70_13<-arrange(sm_awap_ann_70_13,ID,YEAR)
SM_mon_frame<-arrange(SM_mon_frame,ID,YEAR,Month)
SM_ann_frame<-arrange(SM_ann_frame,ID,YEAR)

##Omitting missing and abnormal data
	
	## Climate data 
	climate_mon_frame_70_13$Temp[climate_mon_frame_70_13$Temp< -20]<-NA
	
## Using f_m2y(data,fun="mean"or"sum") (default is mean) to calculte annual data from monthly database

	## NDVI --- mean
	NDVI_ann_82_13<-f_m2y(NDVI_mon_82_13)  

	## PRE --- sum
	PRE_ann_70_13<-f_m2y(climate_mon_frame_70_13[1:4], fun="sum")

	##TEMP --- mean
	Temp_ann_70_13<-f_m2y(climate_mon_frame_70_13[c(1:3,5)]) 

## Using f_summary function to output summary infos of all variables in memory
f_summary()

} ## ------this is end for comment

## Change point detection of NDVI monthly data

load("NDVI_mon_82_13.RData")

year_start=1982
year_end=2013

NDVI_matrix<-matrix(NDVI_mon_82_13$NDVI, nrow=((year_end-year_start+1)*12))
NDVI_frame<-as.data.frame(NDVI_matrix)
NDVI_frame<-NDVI_frame[,1:282200]
	rm(NDVI_mon_82_13, NDVI_matrix)
	gc()




# set doParallel
	
## decide using doparallel or snowfall
par<-"doparallel" # "doparallel" or "snow" or "ff" or "foreach"

if (par=="doparallel"){
	print("using doParallel to simulate")
	
	#clusterEvalQ(cl, library(rms)) # load required packages "rms"
	cl<-makeCluster(25, type="FORK", outfile = "parallel_12_mag.txt")  # set up parallel 
	print(mem_used())#detectCores()-1
#	cl<-makeCluster(detectCores()-1)  # set up parallel 
	print(detectCores())	
#	clusterExport(cl,c("x"))    # share default data for all threads

	system.time(
	STA<-parLapply(cl, NDVI_frame, f_dp, year_start=1982, year_end=2013) # using parLapply to simulate data in parallel way
	)
	## combin general returned data
	STA<-do.call(rbind,STA) 
	save(STA,file="STA_12_mag.RData")
	stopCluster(cl)
	
}else if (par=="foreach"){
	
	library(foreach)
	library(doParallel)
	print("using foreach package to simulate")
	
	cl<-makeCluster(detectCores()-1, type="FORK", outfile = "foreach_debug.txt")  # set up parallel
	registerDoParallel(cl)
	
	STA<-foreach(n=1:564400, 
          .combine = list,
        #  .export = "x",
          .packages = c("pryr", "bfast","trend")
          )  %dopar%  {
  			tryCatch({
    		f_dp(n, data=x, year_start=1982, year_end=2013)
  			}, error = function(e) return(paste("The variable '", n, "'", " caused the error: '", e, "'")))
	}
	## combin general returned data
	##STA<-do.call(rbind,STA) 
	save(STA,file="STA.RData")
	stopCluster(cl)	
	
}else if (par=="snow"){
	print("using snow package to simulate")
	
	lnxOptions <-list(host = "itasca", rscript = "/group/director1234/software/zeus/apps/gcc/4.8.3/r/3.2.3/lib64/R/bin/Rscript", snowlib = "/home/nliu/R/x86_64-pc-linux-gnu-library/3.2")
	cl <- makeCluster(c( rep(list(lnxOptions), 2)), type = "SOCK")
	x<-NDVI_mon_82_13$NDVI
	
	nc<-length(cls)
	
	clusterExport(cl,c("x"))    # share default data for all threads

	system.time(
	STA<-parLapply(cl,seq(1,564400),f_dp,data=x,year_start=1982,year_end=2013) # using parLapply to simulate data in parallel way
	)
	## combin general returned data
	STA<-do.call(rbind,STA) 
	save(STA,file="STA.RData")
	stopCluster(cl)

}else{
	library(ff)
	library(snow)
	library(snowfall)
	print("using snowfall and ff package to simulate")
	
	data<-as.ff(x)
	cores<-40
	sfInit(parallel=TRUE, cpus=cores, socketHosts=c( rep("localhost", cores)), type="SOCK", slaveOutfile="snowcash.txt")
	sfLibrary(ff)
	sfLibrary(bfast)
	sfLibrary(trend)
	#sfExport("x")
	sfExportAll()
	sfClusterSetupRNG()
	print("starting to parallel")
	print(mem_used())
	system.time(ls<-sfLapply(1:56, f_dp,data=x,year_start=1982,year_end=2013))
	la<-do.call("rbind",ls)
	save(la,file="mk.RData")
	sfStop()
}

if (0){

ls<-lapply(c(1:564400), f_change,data=ffdf_NDVI,year_start=1982,year_end=2013,variable="NDVI")

f_change1<-function(n,year_start,year_end,variable){
	require(bfast)
	.start<-(n-1)*((year_end-year_start+1)*12)+1
	
	.end<-n*((year_end-year_start+1)*12)
	print(names(data))
	
	.sublinshi<-ffdf_NDVI[1,3]
	if(0){
	print(.sublinshi)
	print(length(na.omit(.sublinshi))>=12)
	if (!any(is.na(.sublinshi))){
	.linshi<-ts(.sublinshi,frequency = 12,start = c(year_start,1))
	print(.linshi)
	rdist<-12/length(.linshi)
	fit <- bfast(.linshi,h=rdist, season="harmonic", max.iter=1,breaks=2)
	.out<-fit$output[[1]]
	if ( is.list(.out$bp.Vt)){.trend_change<-.out$bp.Vt$breakpoints}else{.trend_change<-NA}
	if ( is.list(.out$ci.Wt)){.season_change<-.out$ci.Wt[[1]][2]}else{.season_change<-NA}
	}else{
	.trend_change<-NA
	.season_change<-NA
	}
	}
	print(n)
	return(str(.sublinshi))
	#return(list(n,.trend_change,.season_change))
}

f_change1(1785,data=ffdf_NDVI,year_start=1982,year_end=2013,variable="NDVI")


load("RESULT_MJ_LCMerge.RData")
load("Carbon_ann_MJ.RData")
load("Carbon_ann_MJ_LCmerge.RData")
Carbon_ann_LC_merge<-Carbon_ann


print("Finish reading ENVI data")
parameters<-read.delim(args[3],sep = " ",header = TRUE)
YEAR_START<-args[4]
print("the first year for WaSSI input is:")
print(YEAR_START)
YEAR_END<-args[5]
print("the last year for WaSSI input is:")
print(YEAR_END)
print(parameters)
#rm(list=ls())

# read rows and cols of the study area
nrows<-nrow(da)
ncols<-ncol(da)
print("nrows=")
print(nrows)
print("ncols=")
print(ncols)
# set default parameters for data processing
## set the start and end year of input data for climate, LAI and land cover data
S_y<-parameters[1,1]
E_y<-parameters[2,1]
S_y_LAI<-parameters[3,1]
E_y_LAI<-parameters[4,1]
S_y_LC<-parameters[5,1]
E_y_LC<-parameters[6,1]

## set the start line for each parameters of the ENVI data

Line_DEM<-parameters[7,1]
Line_S_PRE<-parameters[8,1]
Line_E_PRE<-parameters[9,1]
Line_S_TEMP<-parameters[10,1]
Line_E_TEMP<-parameters[11,1]
Line_S_LAI<-parameters[12,1]
Line_E_LAI<-parameters[13,1]
Line_S_LC<-parameters[14,1]
Line_E_LC<-parameters[15,1]
Line_S_SOIL<- parameters[16,1]

## the coordation of the left-up pixel and the size of pixel  
S_lat<- parameters[17,1]
cell_size<-parameters[18,1]
S_long<- parameters[19,1]
#----------

#---- read each variables
print("read variables from ENVI data")
Pre<-as.vector(da[,,Line_S_PRE:Line_E_PRE])
Temp<-as.vector(da[,,Line_S_TEMP:Line_E_TEMP])
LAI<-as.vector(da[,,Line_S_LAI:Line_E_LAI])
LAI[LAI>=130]<-0 
LC<-as.vector(da[,,Line_S_LC:Line_E_LC])
S_soil<-Line_S_SOIL
VEG<-as.vector(da[,,Line_S_LC-2])
ALT<-as.vector(da[,,Line_DEM])
##--- whether data needs to be transfered by dividing 10
if  (parameters[20,1]) { 
	Pre<-Pre/10.0
	Temp<-Temp/10.0}
if  (parameters[21,1]) { LAI<-LAI/10.0}
if  (parameters[22,1]) { LC<-LC/10.0}
if  (parameters[24,1]) { ALT<-ALT/10.0}

#rm(da)
#gc()

#process climate data------------------------------

Year_C<-rep(c(S_y:E_y), each=nrows*ncols*12)
ID_C<-rep(c(1:(nrows*ncols)),12*(E_y-S_y+1))
Month_C<-rep(rep(c(1:12), each=nrows*ncols),E_y-S_y+1)
data_climate<-data.frame(ID=ID_C,YEAR=Year_C,Month=Month_C,Pre=Pre,Temp=Temp)
system.time(
data_climate<-arrange(data_climate,ID,YEAR,Month))
# system.time(
# with(data_climate,data_climate<<-data_climate[order(ID,YEAR,Month),]))
print(summary(data_climate))
data_climate<-subset(data_climate,YEAR>=YEAR_START & YEAR<=YEAR_END)
write.table(data_climate,"INPUTS/CLIMATE.TXT",sep = ',',row.names = FALSE,col.names=FALSE)
print("Finish climate data")
rm(Year_C,ID_C,Month_C,Pre,Temp)
#gc()
#------------------------------

Year_LAI<-rep(c(S_y_LAI:E_y_LAI), each=nrows*ncols*12)
ID_LAI<-rep(c(1:(nrows*ncols)),12*(E_y_LAI-S_y_LAI+1))
Month_LAI<-rep(rep(c(1:12), each=nrows*ncols),E_y_LAI-S_y_LAI+1)
data_LAI<-data.frame(ID=ID_LAI,YEAR=Year_LAI,Month=Month_LAI,LAI=LAI)
data_LAI<-arrange(data_LAI,ID,YEAR,Month)
print(summary(data_LAI))
data_LAI<-subset(data_LAI,YEAR>=YEAR_START & YEAR<=YEAR_END)
write.table(data_LAI,"INPUTS/LANDLAI.TXT",sep = ',',row.names = FALSE,col.names=FALSE)
print("Finish LAI data")
rm(Year_LAI,ID_LAI,Month_LAI,LAI)
#gc()
#------------------------------

SOIL<-data.frame(ID=c(1:(nrows*ncols)),UZTWM=as.vector(da[,,S_soil]),UZFWM=as.vector(da[,,S_soil+1]),UZK=as.vector(da[,,S_soil+2]),ZPERC=as.vector(da[,,S_soil+3]),REXP=as.vector(da[,,S_soil+4]),LZTWM=as.vector(da[,,S_soil+5]),LZFSM=as.vector(da[,,S_soil+6]),LZFPM=as.vector(da[,,S_soil+7]),LZSK=as.vector(da[,,S_soil+8]),LZPK=as.vector(da[,,S_soil+9]),PFREE=as.vector(da[,,S_soil+10]))
print(summary(SOIL))
for ( i in 2:12){ SOIL[[i]][is.na(SOIL[[i]])]<- -999}
if  (parameters[23,1]) { SOIL<-SOIL/10;SOIL$ID<-c(1:(nrows*ncols))}
write.table(SOIL,"INPUTS/SOILINFO.TXT",sep = ',',row.names = FALSE,col.names=FALSE)
print("Finish SOILinfo data")
#------------------------------

LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
data_cell<-data.frame(ID=c(1:(nrows*ncols)),LAT=LAT,LONG=LONG,VEG=VEG,ALT=ALT)
print(summary(data_cell))
write.table(data_cell,"INPUTS/CELLINFO.TXT",sep = ',',row.names = FALSE,col.names=FALSE)
print("Finish cell info data")
#------------------------------

Year_LC<-rep(c(S_y_LC:E_y_LC), each=nrows*ncols)
ID_LC<-rep(c(1:(nrows*ncols)),(E_y_LC-S_y_LC+1))
data_LC<-data.frame(ID=ID_LC,YEAR=Year_LC,VEG=LC)
data_LC<-arrange(data_LC,ID,YEAR)
print(summary(data_LC))
data_LC<-subset(data_LC,YEAR>=YEAR_START & YEAR<=YEAR_END)
write.table(data_LC,"INPUTS/VEGINFO.TXT",sep = ',',row.names = FALSE,col.names=FALSE)
print("Finish LUCC data")
rm(Year_LC,ID_LC,LC)
print("finsh data process")
}
