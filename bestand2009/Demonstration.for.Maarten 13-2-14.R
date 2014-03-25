# Demonstration for Maarten

# I began with your code for simulating the data, but added to
# it a method for simulating detention length.  (There are
# side effects to this and we can discuss different ways of
# doing it if you think we should, but that's not relevant
# here.)
##################
### Simulation ###
##################

# N    <- 1000
# T    <- 365

# beta <- c(-7,.5)
# x    <- matrix(rnorm(N,.5),ncol=1)
# X    <- cbind(1,x)
# rate <- exp(X%*%beta)

# events  <- matrix(0,N,T)
# testval <- matrix(runif(N*T,0,1),N,T)

# for(i in 1:N){
#     j <- 1
#     while(j <= T-1){
#         if(testval[i,j]<rate[i]){

#             events[i,j]<-1

#             if(rbinom(1, 1, .238) == 1){
#                 detention.length  <- rnbinom(1, size = 4.7, mu = exp(5.059))
#                 events[i, min(c(365, (j+1))):min(c(365, j+detention.length))] <- 3
#             } else{
#                 j <- j + 1
#             }
#         } else{
#             j <- j + 1
#         }
#     }
# }

########################
# ### Making it wide ###
########################

# Y      <- rowSums(events == 1)
# Tact   <- 365 - rowSums(events == 3)
# f.data <- cbind(Y, X, Tact)
# t.data <- f.data[f.data[, 1] != 0, ]


###################################
### Here you can load the data ####
###################################

# setwd("~/Dropbox/NWO Onderzoekstalent/bestand2009/")
# write.table(t.data, file = "datasets/truncdata.dat")
t.data <- read.table("datasets/truncdata.dat")

################
### Analysis ###
################
# So there was in fact a problem with the code that I had translated. I'm not
# sure why it was how it was, but based on what I changed, it had to have been
# related either to using the old beta values (which really shouldn't've caused
# such a big difference), having different convergence criteria, or not having
# it in a data matrix format.

# I found it after I simulated the data (with the above method) and tested it.
# When I tested it with a covariate, everything was fine and I got the same
# values.  When I tested an "intercept-only" model, I immediately had differing
# values, so it was an a-ha! moment.  So I dug in and fixed the two things I
# mentioned earlier and suddenly everything was working perfectly and giving
# the same results.

# For ease of reading, I just source the file.  It's in the directory if you
# want to see what changes were made, but no math is different or anything.
source("Duplicating.Results.REM.R")
TruncPoisFit(t.data, covs = 2)    # Intercept-only
REMFit(t.data, covs = 2)          # Intercept-only
TruncPoisFit(t.data, covs=c(2,3)) # With covariate
REMFit(t.data, covs=c(2,3))       # With Covariate

# As you can see, the output is the same when w and v are held at 1. If you
# vary w and v together, you get the same parameter estimates and Nhat
# but the constant changes, which should be correct... I could probably
# do more to change it such that it's more consistent.

##########################
### Adding in Time Out ###
##########################

# Time out of the population was modeled in the simulated data to try to
# approximate what's in our data set.  There was a ~ .24 chance of being
# arrested, with a negative binomial (it fit better than truncated poiss)

TruncPoisFit(t.data, covs=c(2,3)) # With covariate
REMFit(t.data, covs=c(2,3), w = t.data$Tact, v = 365)       # With Covariate
