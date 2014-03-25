setwd("~/Dropbox/Internship/Population Estimate/R")
source("Recurrent.Events.Data.Setup.R")
b2009 <- read.table("datasets/bestand2009.dat")
require(survival)

# # Testing optim() for Poisson
#     poisson.lik <- function(mu, y){
#         n    <- length(y)
#         logl <- sum(y) * log(mu) - n * mu
#         return(-logl)
#     }

# optim(par = 2, poisson.lik, y=data$aantalregistraties)

# # Testing optim() for ztpr
# zptr.lik <- function(mu, x, Y){

#     lambda <- exp(x%*%mu)
#     logl   <- sum(Y * log(lambda) - lambda - log(1 - exp(-lambda)) - lgamma(Y + 1))
#     return(-logl)
# }

# res <- optim(rep(-1, 5), fn = zptr.lik, x = x, Y = Y, method = "BFGS", control=list(maxit=8000))

# Testing optim() for Maarten's REM

w <- b2009[, 3] # Actual time at risk
v <- b2009[, 4] # Potential time at risk
y <- b2009[, 1] # Number of events
x <- data.matrix(b2009[, 7:ncol(b2009)]) # Covariates

maarten.rem.lik <- function(mu, x, y, w, v) {
    u    <- exp(x%*%mu)
    vu   <- v*u
    wu   <- w*u
    p    <- u^y*exp(-wu)
    p0   <- exp(-vu)
    logl <- colSums(log(p) - log(1 - p0))
    return(-logl)
}

res <- optim(rep(.01, 14), fn = maarten.rem.lik, x = x, y = y, w = 1, v = 1, method="CG", control=list(maxit=8000))
res <- nlm(f = maarten.rem.lik, p = rep(.01, 14), x = x, y = y, w = w, v = v, iterlim=5000)



# Setting up my own REM likelihood


# N_i(t) is the number of events over (0, t] for unit i.
# Unit i is potentially observed over the interval (0, tau_i]
# where that is the observation or truncation time.
# M units, i
# Objective is to estimate the mean function
# Lambda(t) = E{N_i(t)} and the corresponding rate, defined
# as lambda(t) = Lambda'(t) for continuous and
# lambda(t) = E{N_i(t) - N_i(t-1)} when time is discrete.

# We're assuming a closed population for the time being,
# meaning that the interval is just to 365 for everyone


# TESTING SITE
days_in_sample *(log(baseline_hazard) +
                gender * b_gender +
                origin * b_origin) +
for (t in 1:T) {

}

for (t in 1:Tmax) {
    #n.(t) * log lambda(t)
    #n.(t) = sum(deltai(t)*ni(t))
    #deltai(t) = 1 for Ti>=t, else 0
    for (i in m) {
        (last_day[m] >= t) * obs_dag[m] == t)
    }

}

for (t in 1:Tmax) {
    for (i in 1:m) {
        # deltai(t) * lambda(t)
    }
}

for (i in 1:m) {
    # log(1-exp(-Lambda(Ti)))
}

