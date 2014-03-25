setwd("~/Dropbox/NWO Onderzoekstalent/BasicModel")

#source("long.to.matrix_f.R") # make_ev.matrix, make_cov.matrix, f, rem.tvc.lik Old versions with a covariate matrix
source("long.to.array_f.R") # This has makeEventMatrix(), makeCovariateArray() and rem.mvtc.lik
source("wide.form.rem_f.R") #REMFit(), TruncPoisFit()
source("sims.time.out.tvc_f.R") # simTop(N, Time, x, b)

res <- simMoreCovs(N = 1000, Time = 365, cov.val = c(0, 0), delta = -.2, b = c(-7, .5, -.5) )
long <- res$long
ev.mat <- makeEventMatrix(long)
x.arr  <- makeCovariateArray(long, ev.mat)

optim(c(0,0,0), rem.mtvc.lik, ev.mat = events, x.arr = x.arr, method = "BFGS")
REMFit(t.data, covs = c(2,3), w = t.data[,4], v = 365 )

res <- simTopTvc(N = 1000, Time = 365, b = c(-7.5, .5))
ev.mat <- makeEventMatrix(res$long.data)
x.arr  <- makeCovariateMatrix(res$long.data)
optim(c(0,0), rem.tvc.lik, ev.mat = ev.mat, x.arr = x.arr, method = "BFGS")
