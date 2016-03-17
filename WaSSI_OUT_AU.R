##!/bin/Rscript
print("Starting to process")
#------------------
	## this Rscript is used for reading output result from WaSSI model of AU
	## the input data for this code is generally 6 binary DAT data or TXT data
#---------------

#---------
	## read required parameters for R script
	## parameters are: [1] location of data; [2] Total grids; [3] total years ; [4] Year start for WaSSI; [5] Year End for WaSSI
	
args <- commandArgs(TRUE)

#load all required libraries
	library(caTools)
	library(plyr)
	library(reshape2)
	library(pryr)
	library(ggplot2)
	library(raster)

# set the first args as the data location
	setwd(args[1])
	num_grids<-as.integer(args[2])
	print(args[2])
	num_years<-as.integer(args[3])

#load functions
	source("/home/nliu/CODE/Scripts/Funcs_AU_PRE.R")

# check "./R_result/" directory whether exist for output data
	if (! dir.exists("R_result")){dir.create("R_result", showWarnings = TRUE, recursive = FALSE, mode = "0777")}

# load outputs data and save it as "R_result/RESULT.RData"
	## if this file already exist 
	if (file.exists("R_result/RESULT.RData")){

		print('loading outputs data from R_result/RESULT.RData')
		load("R_result/RESULT.RData")
		print(str(RESULT))
		
	}else{

		print("read data from original outputs data")

		## set up the original parameters for these output data
			#Composition information of the input files
			validation_nums   =   18*num_grids*num_years*12
			soilstorage_nums  =   8*num_grids*num_years*12
			monthflow_nums    =   13*num_grids*num_years*12
			monthcarbon_nums  =   6*num_grids*num_years*12
			annualflow_nums   =   12*num_grids*num_years
			annualcarbon_nums =   5*num_grids*num_years
			hucflow_nums      =   9*num_grids
			huccarbon_nums    =   5*num_grids

		## read all result files (binary) to a list called RESULT, each of this file was stored as a framedata in the list
			names_DAT<-c("ANNUALCARBON.DAT","ANNUALFLOW.DAT","HUCCARBON.DAT","HUCFLOW.DAT","MONTHCARBON.DAT","MONTHFLOW.DAT","SOILSTORAGE.DAT","VALIDATION.DAT")	
			ncols_DAT<-c(5,12,5,9,6,13,8,18)
			nums_DAT<-c(annualcarbon_nums,annualflow_nums,huccarbon_nums,hucflow_nums,monthcarbon_nums,monthflow_nums,soilstorage_nums,validation_nums)
			
			# ceat list RESULT
			RESULT<-list("0")
			#print(RESULT)
			
			# read each of these files
			for (n in 1:6) {
			
				.zz <- file(names_DAT[n], "rb")
				.a<-readBin(.zz, numeric(),nums_DAT[n], size = 4)
				.a_matrix<-matrix(.a,ncol=ncols_DAT[n],byrow=T)
				.a_frame<-as.data.frame(.a_matrix)
				RESULT<-c(RESULT,list(.a_frame))
				close(.zz)
				print(names_DAT[n])
			}

		## name each framedata in list RESULT
			names_list<-c("ANNUALCARBON","ANNUALFLOW","HUCCARBON","HUCFLOW","MONTHCARBON","MONTHFLOW","SOILSTORAGE","VALIDATION")
			names(RESULT)[2:7]<-names_list[1:6]
			## name variables for each framedata
			names(RESULT[["ANNUALFLOW"]])<-c("ID","YEAR","PRE","PET","AET","SUN_ET","RUNOFF","Q_P","ET_P","Q_ET","SNWPCKMON","RFACTOR")
			names(RESULT[["ANNUALCARBON"]])<-c("ID","YEAR","GEP","RECO","NEE")
			names(RESULT[["HUCFLOW"]])<-c("ID","PRE","PET","AET","RUNOFF","Q_P","ET_P","Q_ET_P","N_Y")
			names(RESULT[["HUCCARBON"]])<-c("ID","N_Y","GEP","RECO","NEE")
			names(RESULT[["MONTHFLOW"]])<-c("ID","YEAR","MONTH","PRE","TEMP","SMC","SNWPK","PET","AET","SUN_ET","RUNOFF","BASEFLOW","FLOWMCMMon")
			names(RESULT[["MONTHCARBON"]])<-c("ID","YEAR","MONTH","GEP","RECO","NEE")
		
		## save result data
			save(RESULT,file = "R_result/RESULT.RData")
		
		#---print summary of the result
			print(str(RESULT))
			f_list_summary()
			
		print("Saved Result to R_result/RESULT.RData")
	}

# elimate the abnormal values (-999.0, inf, etc.) of result data
	
	## Put all less than 0 and infinite data as NA
		outs<-c("HUCFLOW","HUCCARBON","ANNUALFLOW","ANNUALCARBON")#,"MONTHFLOW","MONTHCARBON")
		for (out in outs){
			print(out)
			for (i in 2:length(RESULT[[out]])){
				print(names(RESULT[[out]])[i])
				if (! names(RESULT[[out]])[i]=="NEE"){
					abnormal<-which(RESULT[[out]][[i]]<0 | is.infinite(RESULT[[out]][[i]]))
					str(abnormal)
					RESULT[[out]][[i]][abnormal]<-NA
				}	
			}
		}
		
# load info data for AU
	load("../cellinfo.RData")
	load("../landcover.RData")

# Transfer all result data to grid raster stack
		
	info<-c("latmin"=min(data_cell$LAT), "latmax"=max(data_cell$LAT), "longmin"=min(data_cell$LONG), "longmax"=max(data_cell$LONG), "ncols"=680, "nrows"=830, "year_start"=min(RESULT$ANNUALFLOW$YEAR), "year_end"=max(RESULT$ANNUALFLOW$YEAR))
	
	outs<-c("HUCFLOW","HUCCARBON","ANNUALFLOW","ANNUALCARBON")#,"MONTHFLOW","MONTHCARBON")
	
	f_plot(RESULT[[outs[1]]],info=info,annual=FALSE,monthly=FALSE)
	f_plot(RESULT[[outs[2]]],info=info,annual=FALSE,monthly=FALSE)
	f_plot(RESULT[[outs[3]]],info=info,annual=T,monthly=F)
	f_plot(RESULT[[outs[4]]],info=info,annual=T,monthly=F)
	#f_plot(RESULT[[outs[5]]],info=info,annual=T,monthly=T)
	#f_plot(RESULT[[outs[6]]],info=info,annual=T,monthly=T)

	

# --------start to validate and analyze AU result
	
	## load WA Basin boundary and infos and save it to Basin_WA.RData"
	## this data is from WIN database
	
		if (file.exists("Basin_WA.RData")){
			### if this file already exist
			print('loading Basin data from Basin_WA.RData')
			load("Basin_WA.RData")
			load("Station_info.RData")
		}else{
			require(caTools)
			Basin_WA<-read.ENVI("subcatchments_WA_5km")
			Basin_WA<-data.frame(ID=c(1:564400),BASIN=as.vector(Basin_WA))
			Basin_WA[Basin_WA==0]<-NA
			Station_info<-read.csv("WIN_subcatchments_info.txt",header=T)
			save(Basin_WA,file="Basin_WA.RData")
			save(Station_info,file="Station_info.RData")
		}
	
	## load AU Basin boundary and infos and save it to Basin_AU.RData"
	## this data is from water storage database	
		
		if (file.exists("Basin_AU.RData")){
			### if this file already exist
			print('loading Basin data from Basin_AU.RData')
			load("Basin_AU.RData")
			load("Basin_AU_info.RData")
		}else{
			require(caTools)
			Basin_AU<-read.ENVI("swma_Basins_5km")
			Basin_AU<-data.frame(ID=c(1:564400),BASIN=as.vector(Basin_AU))
			Basin_AU[Basin_AU==0]<-NA
			Station_info<-read.csv("Basin_AU_info.txt",header=T)
			save(Basin_AU,file="Basin_AU.RData")
			save(Station_info,file="Basin_AU_info.RData")
		}
		
