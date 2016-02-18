## this is an example of R parallel in doParallel and foreach way

# first creat a funtion for process data and return the targert value

f_trend<-function(n){
  
  require(rkt)  # load required packages
  a<-rkt(lin_ann$YEAR, lin_ann[[n]])
  return(c(n-1,a$sl,a$B)) # return targert values
}

f_NDVI<-function(n){
new<-new.env()
new<-apply(NDVI[,,n:n+1],c(1,2),max)
return(new)
#NDVI_new[,,(n+1)/2]<-apply(NDVI[,,n:n+1],c(1,2),max)
}


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



##---- doParallel way

library(doParallel)
library(pryr)
print("Size of NDVI")
print(mem_used())
cl<-makeCluster(detectCores()-1)  # set up parallel

clusterEvalQ(cl, library(rms)) # load required packages
clusterExport(cl,c("NDVI","NDVI_flag","NDVI_new"))    # share default data for all threads

tr<-parLapply(cl,seq(1,312,2),f_NDVI) # using parLapply to simulate data in parallel way

# if return is a matrix, transfer list to array
a<-array(unlist(tr), dim = c(nrow(tr[[1]]), ncol(tr[[1]]), length(tr)))

stopCluster(cl)

tr<-lapply(c(2:108501),f_trend)		# using lapply to simulate data in general way

ts<-do.call(rbind,tr) # combin returned data

##--- foreach way

library(foreach)
library(doParallel)
cl<-makeCluster(detectCores()-1)  # set up parallel
registerDoParallel(cl)

#start time
strt<-Sys.time()

foreach(n = seq(1,312,2), .combine = rbind) %dopar% {

	NDVI_new[,,(n+1)/2]<<-apply(NDVI[,,n:n+1],c(1,2),max)
	print(n)
    }
print(Sys.time()-strt)
stopImplicite(cl)  
