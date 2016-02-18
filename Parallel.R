## this is an example of R parallel in doParallel and foreach way

# first creat a funtion for process data and return the targert value

f_trend<-function(n){
  
  require(rkt)  # load required packages
  a<-rkt(lin_ann$YEAR, lin_ann[[n]])
  return(c(n-1,a$sl,a$B)) # return targert values in a vector
}

f_NDVI<-function(n){
new<-new.env()
new<-apply(NDVI[,,n:n+1],c(1,2),max)
return(new)		# return targert values in a matrix

}

#---- doParallel way

library(doParallel)
library(pryr) # for object size

cl<-makeCluster(detectCores()-1)  # set up parallel 

clusterEvalQ(cl, library(rms)) # load required packages "rms"
clusterExport(cl,c("NDVI","NDVI_flag","NDVI_new"))    # share default data for all threads

tr<-parLapply(cl,seq(1,312,2),f_NDVI) # using parLapply to simulate data in parallel way

## if return is a matrix, transfer list to array
ts<-array(unlist(tr), dim = c(nrow(tr[[1]]), ncol(tr[[1]]), length(tr)))

## combin general returned data
ts<-do.call(rbind,tr) 

stopCluster(cl)

## using lapply to simulate data in nonparallel way
tr<-lapply(c(2:108501),f_trend)		


#--- foreach way
library(doParallel)

cl<-makeCluster(detectCores()-1)  # set up parallel
registerDoParallel(cl)

##start time
strt<-Sys.time()

foreach(n = seq(1,312,2), .combine = rbind) %dopar% {

	NDVI_new[,,(n+1)/2]<<-apply(NDVI[,,n:n+1],c(1,2),max)
	print(n)
    }
print(Sys.time()-strt)
stopImplicite(cl)  
