source("SIMDATA.R")
source("LOGL.R")

getEstimates <- function(N = 1000, Time = 365, beta = c(-7, -.5, .5), delta = -.5, NV = 1, sim = 25) {

t.sim <- sim

mxpo <- matrix(0, sim, 3)
mrem <- matrix(0, sim, 3)
mtvc <- matrix(0, sim, 3)
nhat <- matrix(0, sim, 3)

for(sim in 1:sim){

    a <- simTop(N,Time,beta,delta,NV)    #

    X0     <- a[[1]]   # covariates at t = 0
    XT     <- a[[2]]   # NxT matrix with time-varying covariate values
    events <- a[[3]]
    y      <- a[[4]]
    ones <- matrix(1, ncol = ncol(XT), nrow = nrow(XT))
    X1 <- matrix(X0[,2], ncol = ncol(XT), nrow = nrow(XT))
    x.arr <- array(c(ones, X1, XT), c(nrow(XT), ncol(XT), 3), dimnames = list(NULL, paste0('d', 1:ncol(XT)), paste0('X', 1:3)))

    out1 <- optim(beta,ztpr, X0 = X0, y = y, method="BFGS")

    out2 <- optim(beta,loglREM, X0 = X0, y = y, Time = Time, method="BFGS")

    out3 <- optim(beta, rem.mtvc.lik, x.arr = x.arr, ev.mat = events, Time = Time, method = "BFGS")

    out1$par[1] <- log(exp(out1$par[1])/365)

    mxpo[sim,] <- out1$par

    mrem[sim,] <- out2$par

    mtvc[sim,] <- out3$par

    nhat[sim,] <- c(getNHat(x.arr, out1$par), getNHat(x.arr, out2$par), getNHat(x.arr, out3$par))

    if(sim%%(sim/100) == 0)
    {
        Sys.sleep(0.1)
        cat((sim/t.sim)*100,"% complete","\n")
    }
    }
c.names <- c("Bias", "RMSE")
r.names <- c("B0", "B1", "B2")

pois <- cbind(getBias(mxpo, beta), getRMSE(mxpo, beta))
colnames(pois) <- c.names
rownames(pois) <- r.names

REM <- cbind(getBias(mrem, beta), getRMSE(mrem, beta))
colnames(REM) <- c.names
rownames(REM) <- r.names

REM.TVC <- cbind(getBias(mrem, beta), getRMSE(mrem, beta))
colnames(REM.TVC) <- c.names
rownames(REM.TVC) <- r.names

return(list(nv = NV, N = N, Time = Time, beta = beta, delta = delta, sim = sim, out.pois = pois, out.REM = REM, out.REM.TVC = REM.TVC, NHat = nhat))
}
