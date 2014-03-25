# simulate data with fixed and time-varying covariate
# assumption: rate changes as result of event

simTop <- function(N = 1000, Time = 365, u = 0, b = c(-6,.5,.5)) {

    beta  <- b
    x     <- matrix(rnorm(2*N, u), ncol = 2)
    X     <- cbind(1, x)
    rate  <- exp(X%*%beta)
    delta <- -0.2

    events  <- matrix(0, N, Time)
    testval <- matrix(runif(N * Time, 0, 1), N, Time)

    rates <- matrix(0,N,Time)
    Xtv   <- matrix(0,N,Time) # matrix with time-varying covariate values

    
    for(i in 1:N){
        for(j in 1:Time){
            if(testval[i,j] < rate[i]){
              events[i,j] <- 1
              Xtv[i,j]    <- X[i,3]
              X[i,3]      <- X[i,3] + delta      # Xtv  changes after event
              rate[i]     <- exp(X[1,]%*%beta)   # rate changes after event
            }else{
              Xtv[i,j]    <- X[i,3]
            }
        }
    }
    list(Xtv=Xtv,events=events,x=x)
}
a <- simTop()

Xtv       <- a[[1]]
eventhist <- a[[2]]
x         <- a[[3]]

n.events <- matrix(rowSums(eventhist),ncol=1)

Xtv    <- Xtv[n.events>0,]
events <- eventhist[n.events>0,]
x0     <- x[n.events>0,]
X      <- x0[,1]
X0     <- cbind(1,x)                 # covariate vector at time = 0

beta <- c(-6,.5,.5)

loglXtv <- function(beta){
  
  logl   <- matrix(0,nrow(events),1)
  
  Lambda      <- matrix(0,nrow(events),365) # total intensity
  Lambda.star <- 365*exp(X0%*%beta)

  for(i in 1:nrow(events)){
    Lambda[i,]   <- exp(beta[1]+beta[2]*X[i]+beta[3]*Xtv[i,])  
    lambda       <- Lambda[i,events[i,]==1]                  # rates at time of events
    
    logl[i]      <- log(prod(lambda))-sum(Lambda[i,])-log(1-exp(-Lambda.star[i]))
    
  }
  return(-sum(logl))
}

optim(beta,loglXtv,method="BFGS")

