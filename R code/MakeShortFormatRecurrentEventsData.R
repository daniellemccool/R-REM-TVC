N    <- 1000
T    <- 365

beta <- c(-1,.5)
x    <- matrix(rnorm(N,.5),ncol=1)
X    <- cbind(1,x)
rate <- exp(X%*%beta)/T



events <- matrix(0,N,T)
testval <- matrix(runif(N*T,0,1),N,T)

for(i in 1:N){
  for(j in 1:T){

	if(testval[i,j]<rate[i])events[i,j]<-1
	}
}


y <- rowSums(events)
table(y)

ypos <- events[y>0,]
dim(ypos)

ytot <-rowSums(ypos) 

eventtimes <- matrix(NA,nrow(ypos),max(y))


for(i in 1:nrow(ypos)){
  
  k <- 1
  
  for(j in 1:ncol(ypos)){
    
    if(ypos[i,j]==1){
      
      eventtimes[i,k]<-j
      k <- k + 1
      
    }
    
    if(k > ytot[i])break
  }
}
