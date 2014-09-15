require(tensor)
# model that takes time variation in 2nd predictor into account
ztpr <- function(b, X0, y) {
  u       <- exp(X0%*%b)
  p0      <- exp(-u)
  pCap    <- 1 - p0
  logl    <- sum(y * log(u) - u - log(pCap) - lgamma(y + 1))

  return(-logl)
}

loglXtv <- function(b, X0, XT, events, Time){

  rate    <- exp(b[1]+b[2]*X0[,2]+b[3]*XT)

  Lambda0 <- Time*exp(X0%*%b)

  Lambda  <- matrix(rowSums(rate),ncol=1)

  lambda  <- matrix(0,nrow(events),1)

  for(i in 1:nrow(events)){

    lambda[i] <- prod(rate[i,events[i,]==1])

  }

  logl  <- log(lambda*exp(-Lambda))-log(1-exp(-Lambda0))

  return(-sum(logl))
}


# model that does not take time variation in 2nd predictor into account

loglREM<- function(b, X0, y, Time = Time){

  lambda <- exp(X0%*%b)

  Lambda <- Time*lambda

  p <- lambda^y*exp(-Lambda)

  q <- exp(-Lambda)

  logl <- log(p)-log(1-q)

  return(-sum(logl))

}
require(tensor)
rem.mtvc.lik <- function(b, ev.mat, x.arr, Time) {
    b <- b
    u.mat <- exp(tensor(x.arr, b, alongA = 3, alongB = 1))
    logl  <- sum(log(u.mat[ev.mat == 1])) -
             sum(u.mat[ev.mat != 3]) -
             sum(log(1 - exp(-rowSums(u.mat))))
#             sum(log(1 - exp(-Time*u.mat[,1])))

    return(-logl)
}

loglTV<- function(b, XO, temp, y){

  lambda <- exp(X0%*%b[-length(b)])

  Lambda <- rowSums(exp(temp*b[length(b)])%x%lambda)

  p <- lambda^y*exp(-Lambda)

  q <- exp(-Lambda)

  logl <- log(p)-log(1-q)

  return(-sum(logl))

}

loglXtv <- function(b, X0, XT, events, Time){

  rate    <- exp(b[1]+b[2]*X0[,2]+b[3]*XT)

  Lambda0 <- Time*exp(X0%*%b)

  Lambda  <- matrix(rowSums(rate),ncol=1)

  lambda  <- matrix(0,nrow(events),1)

  for(i in 1:nrow(events)){

    lambda[i] <- prod(rate[i,events[i,]==1])

  }

  logl  <- log(lambda*exp(-Lambda))-log(1-exp(-Lambda0))

  return(-sum(logl))
}

