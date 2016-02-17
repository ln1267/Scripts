##!/bin/Rscript
print("Starting to process")
#------------------
	## this Rscript is used for reading output result from WaSSI model
	## the input data for this code is generally 6 binary DAT data or TXT data
	## this *.hdr file inlcudes: DEM; Monthly PRE, TEMP, LAI; MODIS land cover; 11 SOIL parameters developed for SMA-SMC 
#---------------

#---------
	## read required parameters for R script
	## parameters are: [1] location of data; [2] file name of input ENVI data; [3] parameters file for ENVI data; [4] Year start for WaSSI; [5] Year End for WaSSI
	
args <- commandArgs(TRUE)

#load all required libraries
library(caTools)
library(plyr)
library(reshape2)
library(plyr)

# set the first args as the data location
setwd(args[1])

if (! dir.exists("R_result")){dir.create("R_result", showWarnings = TRUE, recursive = FALSE, mode = "0777")}

# load outputs data and save it as "R_result/RESULT.RData"
if (file.exists("R_result/RESULT.RData")){
print('loading outputs data from "R_result/RESULT.RData"')
load("R_result/RESULT.RData")
}else{
print("read data from original outputs data")

name_dir<-dir(path=".",include.dirs=TRUE,all.files =TRUE,pattern = "*.TXT",full.names = TRUE)
filenames<-name_dir[c(1,2,3,4,5,6)]

RESULT<-lapply(filenames, read.delim,header = FALSE, sep = ",")
.a<-sapply(strsplit(filenames,"/"),"[",length(strsplit(filenames, "/")[[1]]))
names(RESULT)<-sapply(substr(.a,1,nchar(.a)-4),"[",1)

names(RESULT[["ANNUALFLOW"]])<-c("ID","YEAR","PRE","PET","AET","Sun_ET","RUNOFF","RUN_Pratio","ET_Pratio","RUN_ETRatio","SNWPCKMON","RFACTOR")
names(RESULT[["ANNUALCARBON"]])<-c("ID","YEAR","GEP","RECO","NEE")
names(RESULT[["HUCFLOW"]])<-c("ID","PRE","PET","AET","RUNOFF","Q_P","ET_P","Q_ET_P","N_Y")
names(RESULT[["HUCCARBON"]])<-c("ID","N_Y","GEP","RECO","NEE")
names(RESULT[["MONTHFLOW"]])<-c("ID","YEAR","MONTH","PRE","TEMP","SMC","SNWPK","PET","AET","Sun_ET","RUNOFF","BASEFLOW","FLOWMCMMon")
names(RESULT[["MONTHCARBON"]])<-c("ID","YEAR","MONTH","GEP","REO","NEE")


save(RESULT,file = "R_result/RESULT.RData")
print("Saved Result to R_result/RESULT.RData")
}


load("RESULT_MJ_LCMerge.RData")
load("Carbon_ann_MJ.RData")
load("Carbon_ann_MJ_LCmerge.RData")
Carbon_ann_LC_merge<-Carbon_ann



da<-read.ENVI(args[2])
save(da,file="data.RData")
}

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
