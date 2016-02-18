##!/bin/Rscript
print("Starting to process")
#------------------
	## this Rscript is used for transfering GIMMS NDVI 16 days to monthly data based on it's flag data
	## the original data was obtained from https://nex.nasa.gov/nex/projects/1349/wiki/general_data_description_and_access/
#---------------

#---------
	## read required parameters for R script
	## parameters are: [1] location of data; [2] start year of input ENVI data; [3] End year of ENVI data; [4] whether use flag data for monthly data process; [5] Year End for WaSSI
	
args <- commandArgs(TRUE)

#load all required libraries
library(caTools)
library(plyr)
library(reshape2)
library(pryr)
library(doParallel)


# set the first args as the data location
setwd(args[1])
print(args)

# read input NDVI and NDVI_flag data
NDVI<-read.ENVI("ASIAN_NDVI")
NDVI_flag<-read.ENVI("ASIAN_NDVI_flag")

print("NDVI and NDVI_flag dim")
print(dim(NDVI))
print(dim(NDVI_flag))

# creat a new array to store monthly NDVI
NDVI_new<-array(0,c(dim(NDVI)[1],dim(NDVI)[2],dim(NDVI)[3]/2))
print(dim(NDVI_new))

# process monthly data from 16days data
if (1){
#------parallel
	cl<-makeCluster(detectCores()-1)  # set up parallel
	#clusterEvalQ(cl, library(rms)) # load required packages
	clusterExport(cl,c("NDVI","NDVI_flag"))    # share default data for all threads

	if (length(integer(args[4]))){
	
		f_NDVI<-function(n){
		new<-new.env()
		new<-apply(NDVI[,,n:n+1],c(1,2),max)
		return(new)
		#NDVI_new[,,(n+1)/2]<-apply(NDVI[,,n:n+1],c(1,2),max)
		}
		tr<-parLapply(cl,seq(1,312,2),f_NDVI) # using parLapply to simulate data in parallel way
		NDVI_new<-array(unlist(tr), dim = c(nrow(tr[[1]]), ncol(tr[[1]]), length(tr)))
		print(dim(NDVI_new))
		write.ENVI(NDVI_new,"GMMIS_NDVI_mon_flag")
		stopCluster(cl)

	}else{
		f_NDVI<-function(n){
		new<-new.env()
		new<-apply(NDVI[,,n:n+1],c(1,2),max)

		flag_1<-which(NDVI_flag[,,n]<=2 & NDVI_flag[,,n+1]<=2)
		flag_2<-which(NDVI_flag[,,n]<=2 & NDVI_flag[,,n+1]>2)
		flag_3<-which(NDVI_flag[,,n]>2 & NDVI_flag[,,n+1]<=2)
		flag_4<-which(NDVI_flag[,,n]>2 & NDVI_flag[,,n]<7 & NDVI_flag[,,n+1]>2 & NDVI_flag[,,n+1]<7)
		flag_5<-which(NDVI_flag[,,n]>2 & NDVI_flag[,,n]<7  & NDVI_flag[,,n+1]==2)
		flag_6<-which(NDVI_flag[,,n]==7 & NDVI_flag[,,n+1]>2 & NDVI_flag[,,n+1]<7)
		flag_7<-which(NDVI_flag[,,n]==7 & NDVI_flag[,,n+1]==7)
				
		#NDVI_new[,,(n+1)/2][flag_1 | flag_4]
		new[flag_2 | flag_5]<- NDVI[,,n][flag_2| flag_5]
		new[flag_3 | flag_6]<- NDVI[,,n+1][flag_3 | flag_6]
		new[flag_7]<- -1
		return(new)
		#NDVI_new[,,(n+1)/2]<-apply(NDVI[,,n:n+1],c(1,2),max)
		}
		
		tr<-parLapply(cl,seq(1,312,2),f_NDVI) # using parLapply to simulate data in parallel way

		# if return is a matrix, transfer list to array
		NDVI_new<-array(unlist(tr), dim = c(nrow(tr[[1]]), ncol(tr[[1]]), length(tr)))
		print(dim(NDVI_new))
		write.ENVI(NDVI_new,"GMMIS_NDVI_mon_noflag")
		stopCluster(cl)
		
	}


}else{
#---nonparallel

	if (length(integer(args[4]))){
		## Process 16days to monthly data using flag data
		print("Start to process data with flag data")
		n<-1
		for (year in c(args[2]:args[3])){

			for (month in c(1:12)){
			
			NDVI_new[,,(n+1)/2]<-apply(NDVI[,,n:n+1],c(1,2),max)
			
			flag_1<-which(NDVI_flag[,,n]<=2 & NDVI_flag[,,n+1]<=2)
			flag_2<-which(NDVI_flag[,,n]<=2 & NDVI_flag[,,n+1]>2)
			flag_3<-which(NDVI_flag[,,n]>2 & NDVI_flag[,,n+1]<=2)
			flag_4<-which(NDVI_flag[,,n]>2 & NDVI_flag[,,n]<7 & NDVI_flag[,,n+1]>2 & NDVI_flag[,,n+1]<7)
			flag_5<-which(NDVI_flag[,,n]>2 & NDVI_flag[,,n]<7  & NDVI_flag[,,n+1]==2)
			flag_6<-which(NDVI_flag[,,n]==7 & NDVI_flag[,,n+1]>2 & NDVI_flag[,,n+1]<7)
			flag_7<-which(NDVI_flag[,,n]==7 & NDVI_flag[,,n+1]==7)
			
			#NDVI_new[,,(n+1)/2][flag_1 | flag_4]
			NDVI_new[,,(n+1)/2][flag_2 | flag_5]<- NDVI[,,n][flag_2 | flag_5]
			NDVI_new[,,(n+1)/2][flag_3 | flag_6]<- NDVI[,,n+1][flag_3 | flag_6]
			NDVI_new[,,(n+1)/2][flag_7]<- -1
			
			print(n)
			n<-n+2
			
			}
		}
		
		print(dim(NDVI_new))
		write.ENVI(NDVI_new,"GMMIS_NDVI_mon_flag")
			
	}else{
		## select the maximium value for the 16days as monthly data without using flag data
		print("Start to process data without flag data")
		
		n<-1
		for (year in c(args[2]:args[3])){

			for (month in c(1:12)){
			
			NDVI_new[,,(n+1)/2]<-apply(NDVI[,,n:n+1],c(1,2),max)
			print(n)
			n<-n+2
			
			}
		}
		print(dim(NDVI_new))
		write.ENVI(NDVI_new,"GMMIS_NDVI_mon_noflag")
	}

}