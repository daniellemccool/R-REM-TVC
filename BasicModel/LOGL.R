# model that takes time variation in 2nd predictor into account

loglXtv <- function(b){

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

loglREM<- function(b){

  lambda <- exp(X0%*%b)

  Lambda <- Time*lambda

  p <- lambda^y*exp(-Lambda)

  q <- exp(-Lambda)

  logl <- log(p)-log(1-q)

  return(-sum(logl))

}

f <- function(x, b) {
    exp(x%*%b)
}


rem.mtvc.lik <- function(b, ev.mat, x.arr) {
    b <- b
    u.mat <- apply(x.arr, 2, f, b = b)
    logl  <- sum(log(u.mat[ev.mat == 1])) -
             sum(u.mat[ev.mat != 3]) -
#             sum(log(1 - exp(-1 * rowSums(u.mat))))
             sum(log(1 - exp(-1 * Time*u.mat[,1])))

    return(-logl)
}
