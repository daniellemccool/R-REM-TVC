simTop <- function(N = 10, Time = 30, x = .5, b) {

    beta <- b
    x    <- matrix(rnorm(N, x), ncol = 1)
    X    <- cbind(1, x)
    rate <- exp(X%*%beta)

    events  <- matrix(0, N, Time)
    testval <- matrix(runif(N * Time, 0, 1), N, Time)

    detention.length <- matrix(rnbinom((N*Time), size = 4.7, mu = exp(5.059)), N, Time)
    #detention.length <- 20

    for(i in 1:N){
        j <- 1
        while(j <= (Time-1)){
            if(testval[i,j]<rate[i]){
                events[i,j]<-1
                if(rbinom(1, 1, .238) == 1){
                    events[i, min(c(Time, (j+1))):min(c(Time, j+detention.length[i,j]))] <- 3
                    j <- j + detention.length[i, j]
                }else{
                    j <- j + 1
                }
            }else{
                j <- j + 1
            }
        }
    }

    Y <- rowSums(events == 1)
    Tact <- Time - rowSums(events == 3)
    f.data <- cbind(Y, X, Tact)
    wide.data <- f.data[f.data[, 1] != 0, ]

    events <- cbind(1:nrow(events), events)
    keep.events <- events[rowSums(events[,c(-1)])!=0, ]
    keep.events <- cbind(1:nrow(keep.events), keep.events)[, 1:2]
    colnames(keep.events) <- c("id", "subject")

    day          <- which(events==1,arr.ind=T)
    detention    <- which(events == 3, arr.ind = T)
    days.and.det <- rbind(cbind(day, 1), cbind(detention, 3))
    long         <- data.frame(days.and.det[order(days.and.det[,1]),])
    names(long)  <- c("subject", "day", "status")

    covs <- cbind(1:N, X)
    colnames(covs) <- c("subject", "const", "x1")
    long <- merge(long, covs, by = "subject")
    long <- merge(long, keep.events, by = "subject")
    return(list(wide.data = wide.data, long.data = long))

}

simTopTvc <- function(N = 10, Time = 30, cov.val = .5, b) {

    beta  <- b
    x     <- matrix(rnorm(N, cov.val), ncol = 1)
    X     <- cbind(1, x)
    X.mat <- matrix(x, nrow = N, ncol = Time)
    rate  <- exp(X%*%beta)

    events  <- matrix(0, N, Time)
    testval <- matrix(runif(N * Time, 0, 1), N, Time)

    detention.length <- matrix(rnbinom((N*Time), size = 4.7, mu = exp(5.059)), N, Time)
    #detention.length <- 1

    for(i in 1:N){
        j <- 1
        while(j <= (Time-1)){
            if(testval[i,j]<rate[i]){
                events[i,j]<-1
                if(rbinom(1, 1, .238) == 1){
                    dl <- detention.length[i, j]
                    #dl <- detention.length
                    starts <- min(c(Time, (j + 1)))
                    stops  <- min(c(Time, (j + dl)))
                    events[i, starts:stops] <- 3

                    X[i, 2] <- rnorm(1, cov.val)
                    X.mat[i, j:Time] <- X[i, 2]
                    rate[i] <- exp(X[i, ]%*% beta)
                    j       <- (j + dl)
                }else{
                    X[i, 2] <- rnorm(1, cov.val)
                    X.mat[i, j:Time] <- X[i, 2]
                    rate[i] <- exp(X[i, ]%*% beta)
                    j       <- j + 1
                }
            }else{
                j <- j + 1
            }
        }
    }

    Y <- rowSums(events == 1)
    Tact <- Time - rowSums(events == 3)
    f.data <- cbind(Y, X.mat[, Time], Tact)
    wide.data <- f.data[f.data[, 1] != 0, ]
    colnames(wide.data) <- c("Y", "X1", "Tact")

    rows.in <- which(rowSums(events[,])!=0, arr.ind = T)
    events <- events[rows.in, ]
    covs <- X.mat[rows.in, ]

    event <- which(events == 1, arr.ind = T)
    ev.covs <- covs[event]
    det <- which(events == 3, arr.ind = T)
    det.covs <- covs[det]
    ev.and.det <- rbind(cbind(event, ev.covs, 1), cbind(det, det.covs, 3))
    long <- data.frame(ev.and.det[order(ev.and.det[, 1]), ])
    colnames(long) <- c("id", "day", "X1", "status")

    return(list(long.data = long, wide.data = wide.data, events = events, covs = covs))
}

#res <- simTopTvc(N = 100, Time = 365, b = c(-7, .5))


simMoreCovs <- function(N = 10, Time = 30, cov.val = 0, det.prob = 0,  b) {

    beta  <- b
    num.covs <- length(beta)
    X.arr <- array(1, c(N, Time, num.covs), dimnames = list(NULL, paste0('d', 1:Time), paste0('X', 1:num.covs)))

    for (i in 1:(num.covs-1)) {
        X.arr[,,(i+1)] <- matrix(rnorm(N, cov.val[i]), ncol = 1)
    }

    X <- X.arr[ , 1, ]
    rate  <- exp(X%*%beta)

    events  <- matrix(0, N, Time)
    testval <- matrix(runif(N * Time, 0, 1), N, Time)

    detention.length <- matrix(rnbinom((N*Time), size = 4.7, mu = exp(5.059)), N, Time)
    #detention.length <- 1


    for(i in 1:N){
        j <- 1
        while(j <= (Time-1)){
            if(testval[i,j]<rate[i]){
                events[i,j]<-1
                if(rbinom(1, 1, det.prob) == 1){
                    dl <- detention.length[i, j]
                    #dl <- detention.length
                    starts <- min(c(Time, (j + 1)))
                    stops  <- min(c(Time, (j + dl)))
                    events[i, starts:stops] <- 3

                    X[i, 2] <- rnorm(1, cov.val[1])
                    X.arr[i, (j+1):Time, 2] <- X[i, 2]
                    rate[i] <- exp(X[i, ]%*% beta)
                    j       <- (j + dl)
                }else{
                    X[i, 2] <- rnorm(1, cov.val[1])
                    X.arr[i, (j+1):Time, 2] <- X[i, 2]
                    rate[i] <- exp(X[i, ]%*% beta)
                    j       <- j + 1
                }
            }else{
                j <- j + 1
            }
        }
    }

    Y <- rowSums(events == 1)
    Tact <- Time - rowSums(events == 3)
    f.data <- cbind(Y, X.arr[, Time, ], Tact)
    wide.data <- f.data[f.data[, 1] != 0, ]
    colnames(wide.data) <- c("Y", paste0('X', 1:num.covs), "Tact")

    rows.in <- which(rowSums(events[,])!=0, arr.ind = T)
    events <- events[rows.in, ]
    covs <- X.arr[rows.in, , ]

    ind1 <- which(events == 1, arr.ind = TRUE)
    ind2 <- do.call("rbind", replicate(num.covs, ind1, simplify=F))
    ind3 <- cbind(ind2, rep(1:num.covs, each = nrow(ind1)))

    ev.cov.mat <- matrix(covs[ind3], ncol = num.covs)

    det1 <- which(events == 3, arr.ind = TRUE)
    det2 <- do.call("rbind", replicate(num.covs, det1, simplify = F))
    det3 <- cbind(det2, rep(1:num.covs, each = nrow(det2)))


    ev.and.det <- cbind(ind1, ev.cov.mat, 1)
    long <- data.frame(ev.and.det[order(ev.and.det[, 1]), ])
    colnames(long) <- c("id", "day", paste0("X", 1:num.covs), "status")

    return(list(long.data = long, wide.data = wide.data, events = events, X.arr = X.arr))
}

# res <- simMoreCovs(N = 100, Time = 365, cov.val = c(0, .25), b = c(-3, .5, -.5) )
