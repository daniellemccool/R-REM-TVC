# # Under Poisson distribution
# # Generate own data
# # Matrix 365 loop through it
# # x <- matrix(runif(100, 0, 1), 10)
# # y <- x<.2
# n <- 100
# x1 <- rnorm(n)
# x2 <- rnorm(n)
# mean <- exp(-.25 -.5 * x1 + .5 * x2)
# pois <- rpois(n, mean)

# data <- cbind(1:100, pois, x1, x2)

# glm.nb(pois ~ x1 + x2)


# ?beta
# full.data <- c()

# a <- -2
# b <- c(-1.2, 8, 12, 3.5)
# x <- matrix(rbinom(200*4, 1, .5), ncol=4)
# mu <- exp(mean(a+b*x))
# Y <- rpois(200, mu)

# # 0 = 297

# full.data <- cbind(Y, x)
# tr.data <- full.data[which(full.data[,1]!=0), ]
# Y <- tr.data[, 1]
# x <- cbind(1, tr.data[, 2:5])
# poifit <- TPR(Y, x, rep(0, 5))
# b2 <- glm(Y~x, family="poisson")$coef
# b2
###############################
# Under REM                   #
###############################
#install.packages("complex.surv.dat.sim", dependencies = TRUE)
require(complex.surv.dat.sim)
####################################
# No right-censoring, no covariates
# no detention length, no variation
# 12 percent at least 1 event
# 4 percent at least 2 events
# .4 percent 3 events
####################################
test.data <-
rec.ev.sim(n          = 1000,
            foltime    = 365,                   # Max followup time
            dist.ev    = c("lnorm", "lnorm"),   # dist for time to event
            anc.ev     = c(1.4, .8),            # vec of anc. parm for tte
            beta0.ev   = c(7.5, 5.5),           # starting parm for tte
            dist.cens   = "weibull",            # dist for time to censoring
            anc.cens    = 20, # param 1 for dist.cens
            beta0.cens = 20,  # param 2 for dist.cens
#            z          = c("multinom",  , # heterogeneity
#            beta       = ,         # cov effect for x
#            x          = ,          # cov dist./parm
#            lambda     = ,         # dur. of event/disc. risk time
#            max.ep     = 1,
            priskb     = 1
#            max.old    =
)
test.df <- do.call(rbind, test.data)
head(test.df)

trunc.long.df <- test.df[test.df$time != 365, ]
trunc.wide.df <- reshape(trunc.long.df,
        idvar = "nid",
        v.names = c("time","status", "start", "stop"),
        timevar = "real.episode",
        drop = c("obs.episode", "time2", "start2", "stop2", "old", "risk.bef", "long", "z", "x"),
        direction = "wide")

Y <- apply(trunc.wide.df[, c(3,7,11,15)], 1, sum, na.rm=TRUE)
trunc.wide.df <- cbind(Y, trunc.wide.df)

##################################################
# No right-censoring, 2 not-time-varying covariates
# No detention time, no variation
#

test.data.2 <-
rec.ev.sim(n          = 1000,
            foltime    = 365,                   # Max followup time
            dist.ev    = c("lnorm", "lnorm"),   # dist for time to event
            anc.ev     = c(1.4, .8),            # vec of anc. parm for tte
            beta0.ev   = c(7.5, 5.5),           # starting parm for tte
            dist.cens   = "weibull",            # dist for time to censoring
            anc.cens    = 20, # param 1 for dist.cens
            beta0.cens = 20,  # param 2 for dist.cens
#            z          = c("multinom",  , # heterogeneity
            beta       = list(c(-.2, -.4, -.6), c(1, 2, 3)),         # cov effect for x
            x          = list(c("bern", .5), c("bern", .3)),          # cov dist./parm
#            lambda     = ,         # dur. of event/disc. risk time
#            max.ep     = 1,
            priskb     = 1
#            max.old    =
)

test.2.df <- do.call(rbind, test.data.2)
head(test.2.df)

trunc.2.long.df <- test.2.df[test.2.df$time != 365, ]
trunc.2.wide.df <- reshape(trunc.2.long.df,
        idvar = "nid",
        v.names = c("time","status", "start", "stop"),
        timevar = "real.episode",
        drop = c("obs.episode", "time2", "start2", "stop2", "old", "risk.bef", "long", "z"),
        direction = "wide")

Y <- apply(trunc.2.wide.df[, c(5,9,13,17)], 1, sum, na.rm=TRUE)
trunc.2.wide.df <- cbind(Y, trunc.2.wide.df)
