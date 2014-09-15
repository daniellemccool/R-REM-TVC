# 1st covariate fixed
# 2nd covariate time-varying

simTop <- function(N,Time,b,delta,NormVar=0) {

    n.covs <- length(b) -1
    tvc    <- length(b)



    X0   <- cbind(1,matrix(rnorm(n.covs*N,0,1), ncol=n.covs)) # Nx3 covariate matrix at t=0

    XT   <- matrix(X0[,tvc],N,Time)              # NxT matrix with time-varying covariate x[3] at t=0

    E    <- matrix(0, N, Time)                 # NxT event history matrix

    Xt    <- X0                                # Nx3 time-varying covariate matrix

    rate  <- exp(Xt%*%b)                       # Nx1 rate vector at t=0


    for(i in 1:N){

      for(j in 1:Time){

            if(runif(1) < rate[i]){

              E[i,j]    <- 1

              if(NormVar==1){

                Xt[i,tvc]   <- rnorm(1)          # random change in TV covariate vector day after event

                }else{

                Xt[i,tvc]   <- Xt[i,tvc]+delta     # systematic change in TV covariate vector day after event

                }

              rate[i]   <- exp(Xt[i,]%*%b)   # New rate day after event

              if(j<Time){
                XT[i,(j+1):Time] <- Xt[i,tvc]   # New Xtv-values day after event
              }

            }
        }
    }

    NE  <- matrix(rowSums(E),ncol=1)   # number of events

    X0  <- X0[NE>0,]           # select X0, events > 0
    XT  <- XT[NE>0,]           # select XT, events > 0
    E   <- E[NE>0,]            # select events, events > 0
    NE  <- NE[NE>0,]

    list(X0=X0,XT=XT,E=E,NE=NE)
}




