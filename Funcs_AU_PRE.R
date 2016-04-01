##################################################

# This file includs all functions used for studying the relationship between VEG and PRE in AU

###---------list of functions
#	f_parallel(data=null, fun=null, type="parallel")		for setting up parallel methods
#	f_m2y(data, fun="mean")			for transfering monthly frame data to annual data by using fun="sum" or "mean"	
#	f_summary()					for outputing summary infos of all "data frame objects" in memory
#	f_dp(data,seasonal=TRUE,year_start,year_end)	for seasonal or annual changepoint and MK trend analysis
#	f_plot<-function(data,info,annual=FALSE,monthly=FALSE) for monthly or annual grid data plot
#	f_grid2basin(data,type="annual",fun="mean")	#Transfer grid frame data to basin data by fun="mean"

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

##Transfer grid frame data to basin data by fun="mean"
f_grid2basin<-function(data,type="annual",fun="mean"){
	if(type=="HUC"){
			
		.linshi<-melt(data,id=c(1,2))
		.out<-dcast(.linshi, BASIN~variable, get(fun), na.rm=TRUE) 
		return(.out)
	}else if(type=="annual"){
			
		.linshi<-melt(data,id=c(1,2,3))
		.out<-dcast(.linshi, BASIN+YEAR~variable, get(fun), na.rm=TRUE) 
		return(.out)
			
	}else if(type=="month"){
			
		.linshi<-melt(data,id=c(1,2,3,4))
		.out<-dcast(.linshi, BASIN+YEAR+MONTH~variable, get(fun), na.rm=TRUE) 
		return(.out)
	}
}
## summary funtion which can output summary information for all data frame objects in memory
f_summary<-function(){
	print("print info for all dataframe objects")
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

## summary funtion which can output summary information for all list objects in memory
f_list_summary<-function(){
  print("print info for all list objects")
  a<-ls(envir=.GlobalEnv)
  #print(a)
  for (i in c(1:length(a))){
    if (is.list(get(a[i]))){
      print(a[i])
      str(get(a[i]))
      len<-length(get(a[i]))
      for (j in 1:len){
        if (is.data.frame(get(a[i])[[j]])){
          print(names(get(a[i])[j]))
		  print(summary.data.frame(get(a[i])[[j]]))
        }
      }
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

####################################################################
## this function is used for plot Brick result
## 1, the dataframe data will be transfer to brick
## 2, using ggplot to plot brick
#####################################################################
f_grid_plot<-function(data,info,annual=FALSE,monthly=FALSE){
	#info<-c("latmin"=1, "latmax"=1, "longmin"=1, "longmax"=1, "ncols"=1, "nrows"=1, "nbands"=1,"year_start"=1, "year_end"=1,"annual"=0,"monthly"=0)
	## data is the original data frame and info consists the required infor mation for transfering data from frame to brick
	require(raster)
	require(plyr)
	require(rasterVis)
	require(ggplot2)
	
	.brick.plot<-function(bricks,name){
		
			.gplot<-gplot(bricks) + geom_tile(aes(fill = value)) +
			facet_wrap(~ variable) + #,ncol=3
			#scale_fill_gradient(low = 'white', high = 'blue') +
			scale_fill_gradientn(colours=c("blue","green","yellow","red"),name=name,na.value = "grey70")+ #limits=c(500,1000),
			coord_equal()+
			theme_set(theme_bw())
			
			#ggsave(filename, plot = last_plot(), device = NULL, path = NULL, scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE, ...)
			#ggsave(gplot,file =paste("trend/","P_Q.pdf",sep=""),width = 10,  units = c("cm"),dpi = 300)
			filename<-paste("images/",name,".pdf",sep="")
			ggsave(.gplot,file=filename,width = 20,  units = c("cm"),dpi=300)
			
			#print(class(a))
		}
	
	if(annual){
		
		if(monthly){
	## this is for monthly result
		data<-arrange(data,YEAR,MONTH,ID)
			for (var in 4:length(data)){
				nbands=(info["year_end"]-info["year_start"]+1)*12
				.array<-array(data[[var]],c(info["ncols"],info["nrows"],nbands))
				.brick<-brick(.array,xmn=info["longmin"],xmx=info["longmax"],ymn=info["latmin"],ymx=info["latmax"],crs="+proj=longlat+datum=WGS84")
				names(.brick)<-paste("Monthly",names(data)[var],paste(rep(c(info["year_start"]:info["year_end"]),each=12),c(1:12)))
				print(paste("Monthly",names(data)[var]))
				.name<-paste("Monthly",names(data)[var],sep="_")
				.brick.plot(.brick,.name)	
				
				#print(.brick)
			}
		}else{
	## this is for annual result
		data<-arrange(data,YEAR,ID)
			for (var in 3:length(data)){
			nbands=info["year_end"]-info["year_start"]+1
			.array<-array(data[[var]],c(info["ncols"],info["nrows"],nbands))
			.brick<-brick(.array,xmn=info["longmin"],xmx=info["longmax"],ymn=info["latmin"],ymx=info["latmax"],crs="+proj=longlat+datum=WGS84")
			names(.brick)<-paste("Annual",names(data)[var],c(info["year_start"]:info["year_end"]))
			#print(.brick)
			print(paste("Annual",names(data)[var]))
			.name<-paste("Annual",names(data)[var],sep="_")
			.brick.plot(.brick,.name)
			}
		}
	}else{
	## this is for HUC result
		for (var in 2:length(data)){
			nbands=1
			.array<-array(data[[var]],c(info["ncols"],info["nrows"],nbands))
			.brick<-brick(.array,xmn=info["longmin"],xmx=info["longmax"],ymn=info["latmin"],ymx=info["latmax"],crs="+proj=longlat+datum=WGS84")
			names(.brick)<-paste("HUC",names(data)[var])
			#print(.brick)
			.name<-paste("HUC",names(data)[var],sep="_")
			print(paste("HUC",names(data)[var]))
			.brick.plot(.brick,.name)
			
		}
	}

}

####################################################################
## this function is used for plot scatter valid result
## 1, the dataframe data will be transfer to brick
## 2, using ggplot to plot brick
#####################################################################
f_scatter_plot<-function(data,info,annual=FALSE,monthly=FALSE){
cof<-coef(lm(Q[3:9] ~ Observed[3:9], data = ann_mean_MJ))

ggplot(ann_mean_MJ, aes(x=Observed[3:9], y=Q[3:9])) +
  geom_point(size=4) +    # Use hollow circles
  geom_abline(intercept = cof[1], slope = cof[2]) +   # Don't add shaded confidence region
  #geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  scale_x_continuous(name="Observed annual runoff (mm)") +
  scale_y_continuous(name="Simulated annual runoff (mm)")+#limits=c(300, 700)
  theme(axis.title.x = element_text(family="Times",face="bold", colour="black", size=12),
        axis.title.y  = element_text(family="Times",face="bold", colour="black", size=12),
        axis.text.x  = element_text(family="Times",face="bold",size=10),
        axis.text.y  = element_text(family="Times",face="bold",size=10))+
  annotate("text",family="Times", x = 500, y = 525, label = "Y = 0.94 * X - 93.8", fontface="italic",size=8)+
  annotate("text",family="Times", x = 500, y = 500, label="R^2 = 0.75\n RMSE = 135 mm", size=6)

	#ylab(expression("today's temperature is "*-5~degree*C))
	#qplot(1,1) + ylab(expression(Temp^2))

####-------plot veg water balance box
}

f_box_plot<-function(name1){
  g_plot<-ggplot(data = ann_mean_main_veg, aes(x = VEG, y = ann_mean_main_veg[[a]])) + 
    stat_boxplot(geom = "errorbar", stat_params = list(width = 0.5), geom_params = list()) +
    geom_boxplot() + xlab("Vegetation types") + 
    ylab(name1) + theme_bw(base_size = 16, base_family = "Times")
  ggsave(g_plot,file =paste("box/",names(ann_mean_MJ),".pdf",sep="")[a],dpi = 300)
  #print(g_plot)
}

###   plot annual mean line 
f_line_plot<-function(name1){
  r<-coef(lm(mean_ann_MJ_Y[[a]] ~ YEAR, data = mean_ann_MJ_Y))
  print(r[2])
  l_plot<- ggplot(data = mean_ann_MJ_Y, aes(x = YEAR, y = mean_ann_MJ_Y[[a]])) + geom_point(size=4,shape=21, fill="white") + 
    geom_line(size = 1) + scale_x_continuous(breaks=2002:2014)+
    xlab("YEAR") + ylab(name1) + theme_bw(base_size = 14, base_family = "Times") +
    geom_abline(intercept = r[1], slope = r[2])
  ggsave(l_plot,file =paste("line/",names(mean_ann_MJ_Y),".pdf",sep="")[a],dpi = 300)
  print(names(mean_ann_MJ_Y)[a])
}