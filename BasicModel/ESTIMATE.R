source("SIMDATA.R")
source("LOGL.R")


N     <- 1000
Time  <- 365
beta  <- c(-7,-.5,.5)
delta <- -0.5    # assumes that time-varying covariate changes with delta per event
NV    <- 1       # if 1, time-varying covariate changes randomly, if 0 the change is delta per event
sim   <- 25

mxtv <- matrix(0,sim,3)
mrem <- matrix(0,sim,3)
mtvc <- matrix(0,sim,3)

for(sim in 1:sim){

    a <- simTop(N,Time,beta,delta,NV)    #

    X0     <- a[[1]]   # covariates at t = 0
    XT     <- a[[2]]   # NxT matrix with time-varying covariate values
    events <- a[[3]]
    y      <- a[[4]]
    ones <- matrix(1, ncol = ncol(XT), nrow = nrow(XT))
    X1 <- matrix(X0[,2], ncol = ncol(XT), nrow = nrow(XT))
    x.arr <- array(c(ones, X1, XT), c(nrow(XT), ncol(XT), 3), dimnames = list(NULL, paste0('d', 1:ncol(XT)), paste0('X', 1:3)))

    out1 <- optim(beta,loglXtv,method="BFGS")

    out2 <- optim(beta,loglREM,method="BFGS")

    out3 <- optim(beta, rem.mtvc.lik, x.arr = x.arr, ev.mat = events, method = "BFGS")

    mxtv[sim,] <- out1$par

    mrem[sim,] <- out2$par

    mtvc[sim,] <- out3$par

    }

rnames <- c("min","mean","max")
cnames <- c("b0","b1","b2")

out.mxtv <- rbind(apply(mxtv,2,min),apply(mxtv,2,mean),apply(mxtv,2,max))
out.mrem <- rbind(apply(mrem,2,min),apply(mrem,2,mean),apply(mrem,2,max))
out.mtvc <- rbind(apply(mtvc,2,min),apply(mtvc,2,mean),apply(mtvc,2,max))

colnames(out.mxtv)<-cnames
colnames(out.mrem)<-cnames
colnames(out.mtvc)<-cnames
rownames(out.mxtv)<-rnames
rownames(out.mrem)<-rnames
rownames(out.mtvc)<-rnames

out.mxtv
out.mrem
out.mtvc