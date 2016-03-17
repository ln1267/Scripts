##!/bin/Rscript
print("Starting to process")
#------------------
	## 	this Rscript is used for reading nc result from "Australian Landscape Water Balance"(AULWB)
	##	http://www.bom.gov.au/water/landscape/
	##	Using this result to valid WaSSI-C
#---------------

#---------
	## read required parameters for R script
	## parameters are: [1] location of data; [2] Total grids; [3] total years ; [4] Year start for WaSSI; [5] Year End for WaSSI

#tips this script used RNetCDF package, therefore, the following modules should be loaded in ZEUS in advance

#	module load gcc netcdf udunits r
args <- commandArgs(TRUE)

#load all required libraries
	library(caTools)
	library(plyr)
	library(reshape2)
	library(pryr)
	library(ggplot2)
	library(raster)
	library(RNetCDF)
	library(Hmisc)
	library(caTools)
	
# set the first args as the data location
	setwd(args[1])
	
#load functions
	source("/home/nliu/CODE/Scripts/Funcs_AU_PRE.R")
	
#	Read nc AULWB database and aggregate it to monthly

	##	initiate global parameters
		start_y<-2006
		end_y<-2015
		nrows<-681
		ncols<-841
		vars<-c("rr","e0_avg","qtot_avg")
	for (var in vars){
	##	creat two arrays for storing original nc data and the target transposed data
		data_month<-array(0,c(ncols,nrows,12*(end_y-start_y+1)))
		data_t_mon<-array(0,c(nrows,ncols,12*(end_y-start_y+1)))	
		print(var)
		n<-1
		for (y in start_y:end_y){
			
			if (var =="rr"){
			
				ncvar<-"rain_day"
				
			}else if(var =="e0_avg"){
			
				ncvar<-var
			
			} else if (var =="qtot_avg"){
				
				ncvar<-var
			
			}
			
			ncname<-paste("nc/",var,"_",y,".nc",sep="")	# rr_,e0_avg_,qtot_avg_
			ncfile<-open.nc(ncname)
			#print.nc(ncfile)
			ncdata<-var.get.nc(ncfile,ncvar) # rain_day,e0_avg,qtot_avg
			end_m<-0
			print(y)
			
			for (m in 1:12){
				
				date<-paste(y,"-",m,"-","01",sep="")
				days<-monthDays(as.Date(date))
				start_m<-end_m+1
				end_m<-end_m+days
				data_month[,,n]<-apply(ncdata[,,start_m:end_m],c(1,2),sum,na.rm=T)
				n<-n+1
				print(m)
			}
		  
		}
		
	##	transpose data to target result	
		for (i in 1:120){data_t_mon[,,i]<-t(data_month[,,i])}
		
	##	Write result to envi data and RData
		if (var =="rr"){
			rr_mon<-data_t_mon[1:680,11:840,]
			write.ENVI(rr_mon,"rr_06-15_mon")
			save(rr_mon,file="rr_mon.RData")
			
		}else if(var =="e0_avg"){
			aet_mon<-data_t_mon[1:680,11:840,]
			write.ENVI(aet_mon,"aet_mon_06-15_mon")
			save(aet_mon,file="aet_mon.RData")
			
		} else if (var =="qtot_avg"){
			runoff_mon<-data_t_mon[1:680,11:840,]
			write.ENVI(runoff_mon,"runoff_06-15_mon")
			save(runoff_mon,file="runoff_mon.RData")
		}
			
	}

#	transfer grid result to dataframe and save it to "AULWB_frame_mon.RData" and "AULWB_frame_ann.RData"
	nrows<-680
	ncols<-830
	
	AULWB_frame_mon<-data.frame(ID=rep(c(1:(nrows*ncols)),12*(end_y-start_y+1)),YEAR=rep(c(start_y:end_y), each=nrows*ncols*12),Month=rep(rep(c(1:12), each=nrows*ncols),(end_y-start_y+1)),RR_LWB=as.vector(rr_mon),AET_LWB=as.vector(aet_mon),Q_LWB=as.vector(runoff_mon))

	AULWB_frame_mon<-arrange(AULWB_frame_mon,ID,YEAR,Month)
	
	AULWB_frame_ann<-f_m2y(AULWB_frame_mon, fun="sum")
	
	save(AULWB_frame_mon,file="AULWB_frame_mon.RData")
	save(AULWB_frame_ann,file="AULWB_frame_ann.RData")

	
	