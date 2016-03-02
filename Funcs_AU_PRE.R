##################################################

# This file includs all functions used for studying the relationship between VEG and PRE in AU


###---------list of functions
#	f_parallel(data=null, fun=null, type="parallel")		for setting up parallel methods
#	f_m2y(data, fun="mean")			for transfering monthly frame data to annual data by using fun="sum" or "mean"	
#	f_summary()					for outputing summary infos of all "data frame objects" in memory
#	f_dp(data,seasonal=TRUE,year_start,year_end)	for seasonal or annual changepoint and MK trend analysis

##################################################

## setup parallel for "parallel" or "doParallel" or "foreach" or "snow"

f_parallel<-function(data=null, fun=null, type="parallel"){
	
	if (type=="parallel"){
	
	library(parallel)
	print("using parallel package to simulate")
	print(paste("Num of Cores=", detectCores()))
	
	## set up parallel type to "FORK", all cluster share the global variables 
	cl<-makeCluster(detectCores()-1, type="FORK")  

	}else if (type=="doParallel"){
	
	library(doParallel)
	print("using doParallel package to simulate")
	print(paste("Num of Cores=", detectCores()))
	
	cl<-makeCluster(detectCores()-1, type="FORK")  # set up parallel 
	clusterEvalQ(cl, library(rms)) # load required packages "rms"
	
	print(mem_used())
#	cl<-makeCluster(detectCores()-1)  # set up parallel 
	print(detectCores())	
#	clusterExport(cl,c("x"))    # share default data for all threads

	}else if (type=="foreach"){
	
	library(foreach)
	library(doParallel)
	print("using foreach package to simulate")
	
	cl<-makeCluster(detectCores()-1,outfile = "foreach_debug.txt")  # set up parallel
	registerDoParallel(cl)
	
	foreach(exponent = 2:4, 
          .combine = c,
          .export = "base",
          .packages = c("rms", "mice")
          )  %dopar%  {
  			tryCatch({
    		c(1/x, x, 2^x)
  			}, error = function(e) return(paste0("The variable '", x, "'", " caused the error: '", e, "'")))
}
    	
	stopCluster(cl)

	}else if (type=="snow"){



	}else if (type=="snow"){
	
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
	print("using snowfall and ff package to simulate")
	x<-as.ffdf(NDVI_mon_82_13)
	cores<-detectCores()-1
	sfInit(parallel=TRUE, cpus=cores, type="SOCK")
	sfLibrary(ff)
	sfLibrary(bfast)
	sfLibrary(trend)
	sfExport("x")
	sfClusterSetupRNG()
	#system.time(ls<-sfLapply(1:564400, f_change,data=x,year_start=1982,year_end=2013,variable="NDVI"))
	system.time(ls<-sfLapply(1:564400, f_mk,data=x,year_start=1982,year_end=2013,variable="NDVI"))
	la<-do.call("rbind",ls)
	save(la,file="mk.RData")
	sfStop()
}



}



##Transfer monthly frame data to annual data by fun="sum" ot "mean"
f_m2y<-function(data, fun="mean"){
  
  .linshi<-melt(data,id=c(1,2,3))
  .out<-dcast(.linshi, ID+YEAR~variable, get(fun), na.rm=TRUE) 
  return(.out)
  
}

## summary funtion which can output summary information for all data frame objects in memory
f_summary<-function(){
	print("print info for all objects")
	a<-ls(envir=.GlobalEnv)
	print(a)
	for (i in c(1:length(a))){
		if ( is.data.frame(get(a[i]))){
		print(a[i])
		str(get(a[i]))
		print(summary.data.frame(get(a[i])))
		}
  }

}  

####################################################################
## changepoint detection using "bfast" package and MK test using "trend" package
## in seasonal (default) and annual scale 
## changepoint detection using "bfast" package 
## http://www.sciencedirect.com/science/article/pii/S003442570900265X
######################################################################

f_dp<-function(data,seasonal=TRUE,year_start,year_end){
	require(bfast)
	require(trend)
	require(pryr)
	#.start<-(n-1)*((year_end-year_start+1)*12)+1
	#.end<-n*((year_end-year_start+1)*12)
	#print(n)
	print(mem_used())
	
	if(seasonal){
		# seasonal changepoint and trend detection
		print("seasonal scale process")
		# IF all data are NA or equal, there would be no trend
		if (!(any(is.na(data)) | isTRUE(all.equal(data, rep(data[1], length(data)))))){
		.linshi<-ts(data,frequency = 12,start = c(year_start,1))
		
		rdist<-12/length(.linshi)
		# ratio of distance between breaks (time steps) and length of the time series
		
		fit <- bfast(.linshi,h=rdist, season="harmonic", max.iter=1,breaks=2)
		.out<-fit$output[[1]]
		if ( is.list(.out$bp.Vt)){.trend_change<-.out$bp.Vt$breakpoints}else{.trend_change<-NA}
		if ( is.list(.out$ci.Wt)){.season_change<-.out$ci.Wt[[1]][2]}else{.season_change<-NA}
		
		## MK trend detection using "trend" package 
		
		.outmk<-smk.test(.linshi)
		.outslope<-sea.sens.slope(.linshi)

		}else{
		
		##---change point detect result
		
		.trend_change<-NA
		.season_change<-NA
		
		## MK test result
		
		.outmk<-data.frame(tautot=NA,pvalue=NA)
		.outslope<-data.frame(b.sen=NA)
		
		}
		return(list(CP_trend=.trend_change,CP_season=.season_change,TAU=.outmk$tautot,PMK=.outmk$pvalue,SLOPE=.outslope$b.sen))
	}else{
		# annual changepoint and trend detection
		print("annual scale process")
		# IF all data are NA or equal, there would be no trend
		if (!(any(is.na(data)) | isTRUE(all.equal(data, rep(data[1], length(data)))))){
		.linshi<-ts(data,frequency = 1, start = year_start)
		
		rdist<-3/length(.linshi)
		#print(.linshi)
		fit <- bfast(.linshi,h=rdist,season = "none", max.iter=1,breaks=2)
		
		.out<-fit$output[[1]]
		
		if ( is.list(.out$bp.Vt)){.trend_change<-.out$bp.Vt$breakpoints}else{.trend_change<-NA}
				
		## MK trend detection using "trend" package 
		
		.outmk<-mk.test(.linshi)
		.outslope<-sens.slope(.linshi)

		}else{
		
		##---change point detect result
		
		.trend_change<-NA
				
		## MK test result
		
		.outmk<-data.frame(tautot=NA,pvalue=NA)
		.outslope<-data.frame(b.sen=NA)
		
		}
		return(list(CP_trend=.trend_change,TAU=.outmk$tautot,PMK=.outmk$pvalue,SLOPE=.outslope$b.sen))
	}
}
