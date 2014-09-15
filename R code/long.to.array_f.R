require("dplyr")
makeEventMatrix <- function(data) {
    N <- length(unique(data$id))
    T <- max(data$day)

    ev.mat <- matrix(0, nrow = N, ncol = T)
    ev.mat[cbind(filter(data, status == 3)[, "id"], filter(data, status == 3)[,"day"])] <- 3
    ev.mat[cbind(filter(data, status == 1)[, "id"], filter(data, status == 1)[,"day"])] <- 1

    return(ev.mat)
}

makeCovariateArray <- function(data, ev.mat){
    N <- length(unique(data$id))
    Time <- max(data$day)
    num.covs <- length(data) - 3

    cov.array <- array(1, c(N, Time, num.covs), dimnames = list(NULL, paste0('d', 1:Time), paste0('X', 1:num.covs)))
    event.days <- filter(data, status == 1)
    core <- subset(event.days, subset = !duplicated(event.days["id"]))

    start.vals <- core[, !colnames(core) %in% c("id", "day", "status")]
    cur.vals <- start.vals
    for (i in 1:N) {
        for (j in 1:Time) {

                if(ev.mat[i, j] == 1){
                    cur.vals[i, ] <- select(filter(data, id == i, day == j), !colnames(data) %in% c("id", "day", "status"))
                }
                cov.array[i, j, ]    <- unlist(cur.vals[i, ])


        }
    }

    return(cov.array)
}



rem.mtvc.lik <- function(b, ev.mat, x.arr) {
    b <- b
    u.mat <- exp(tensor(x.arr, b, alongA = 3, alongB = 1))
    logl  <- sum(log(u.mat[ev.mat == 1])) -
             sum(u.mat[ev.mat != 3]) -
             sum(log(1 - exp(-1 * rowSums(u.mat))))
#             sum(log(1 - exp(-1 * Time*u.mat[,1])))

    return(-logl)
}

getNHat <- function(x.arr, b) {
    u.mat <- exp(tensor(x.arr, b, alongA = 3, alongB = 1))
    p0      <- exp(-1 * ncol(u.mat)*u.mat[,1])
    pCap    <- 1-p0
    Nhat    <- sum(1/pCap)
    return(Nhat)
}
