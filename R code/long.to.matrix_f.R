require("dplyr")
makeEventMatrix <- function(data) {
    N <- length(unique(data$id))
    T <- max(data$day)

    ev.mat <- matrix(0, nrow = N, ncol = T)
    ev.mat[cbind(filter(data, status == 3)[, "id"], filter(data, status == 3)[,"day"])] <- 3
    ev.mat[cbind(filter(data, status == 1)[, "id"], filter(data, status == 1)[,"day"])] <- 1

    return(ev.mat)
}
# Need a change matrix to find out whether it changes or not
# Find out if it's equal to the last
# This is for migrating to a different system where the dates
# are the list elements
# time series aggregation, only keep the last
# saply(unique(a), function(x), max(which(a == x)))
# ind <- which(id !%*% daily[,1])

#daily <- filter(long, day == 1)
#ind <- which(unique(long$id) %w/o% daily$id)

makeCovariateMatrix <- function(data){
    N <- length(unique(data$id))
    T <- max(data$day)

    event.days <- filter(data, status == 1)
    e.day.ind <- event.days$day
    e.sub.ind <- event.days$id
    e.cov     <- event.days$X1

    all.ind   <- cbind(e.sub.ind, e.day.ind, e.cov)
    o         <- order(e.day.ind, e.sub.ind, decreasing = TRUE)
    sorted    <- all.ind[o,]

    core <- subset(event.days, subset = !duplicated(event.days["id"], fromLast = TRUE))
    X.mat <- matrix(core[,"X1"], nrow = N, ncol = T)
    X.mat[cbind(rep(sorted[,1], times = sorted[,2]), sequence(sorted[,2]))] <- rep(sorted[,3], times = sorted[,2])

    return(X.mat)
}

f <- function(x, b) {
    exp(cbind(1, x)%*%b)
}


rem.tvc.lik <- function(b, ev.mat, x.mat) {
    b <- b
    u.mat <- apply(x.mat, 2, f, b = b)
    logl  <- sum((ev.mat == 1) * log(u.mat)) -      # Working
             sum((ev.mat != 3) * u.mat) -           # Working
             sum(log(1 - exp(-1 * rowSums(u.mat)))) # Working

    return(-logl)
}

getNHat <- function(X.arr, b) {

}

# make_u.matrix <- function(X.mat, b) {
#     f <- function(x, b) {
#         exp(cbind(1,x)%*%b)
#     }
#     apply(X.mat, 2, f, b = b)
# }