N    <- 1000
T    <- 365

beta <- c(-7,.5)
x    <- matrix(rnorm(N,.5),ncol=1)
X    <- cbind(1,x)
rate <- exp(X%*%beta)



events <- matrix(0,N,T)
testval <- matrix(runif(N*T,0,1),N,T)

for(i in 1:N){
  for(j in 1:T){

	if(testval[i,j]<rate[i])events[i,j]<-1
	}
   }

s<-0:10
nevents <- rowSums(events)
table(nevents)
zeroevents <- rowSums(events)==0