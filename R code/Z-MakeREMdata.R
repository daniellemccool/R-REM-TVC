# parameters simulated data
N      <- 100000
Time   <- 365
beta   <- c(-1,.5)


X      <- cbind(1,matrix(rnorm(N,.5),ncol=1))
rate   <- exp(X%*%beta)/Time


# make short format data
events         <- matrix(rate,N,Time) > matrix(runif(N*Time,0,1),N,Time)
y              <- rowSums(events)
day            <- which(events[y>0,],arr.ind=T)
colnames(X)    <- paste("x",0:(length(beta)-1),sep="")
colnames(day)  <- c("subject","day")
short.data     <- cbind(y,X)[y>0,]

# make long format data
long           <- data.frame(day[order(day[,1]),])
covs           <- data.frame(cbind((1:dim(short.data)[1]),short.data[,-(1:2)]))
colnames(covs) <- c("subject","X") 
long.data      <- merge(long,covs,by="subject")


# fit Poisson and REM model to short data

source("Z-OneSourceCRC.R")

Pois(short.data,1,2:3)
REMshort(short.data,1,2:3,0,0)