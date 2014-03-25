# simulate data with fixed and time-varying covariate


simTop <- function(N = 1000, Time = 365, u = 0, b = c(-6,.5,.5)) {

    beta  <- b
    x     <- matrix(rnorm(2*N, u), ncol = 2)
    X     <- cbind(1, x)
    rate  <- exp(X%*%beta)

    events  <- matrix(0, N, Time)
    testval <- matrix(runif(N * Time, 0, 1), N, Time)

    #detention.length <- matrix(rnbinom((N*Time), size = 4.7, mu = exp(5.059)), N, Time)
    #detention.length <- 20

    for(i in 1:N){
        for(j in 1:Time){
            if(testval[i,j] < rate[i]){
                events[i,j] <- 1
                X[i,3]      <- X[i,3] + delta
                rate[i]     <- exp(X[1,]%*%beta)
            }
        }
    }
}


simMoreCovs <- function(N = 10, Time = 30, cov.val = 0, delta = -.02, det.prob = 0,  b) {

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
                # Event
                events[i,j]<-1
                X[i, 2] <- X[i, 2] + delta
                X.arr[i, (j+1):Time, 2] <- X[i, 2]
                rate[i] <- exp(X[i, ] %*% beta)
                if(rbinom(1, 1, det.prob) == 1){
                    # Detention time
                    dl <- detention.length[i, j]
                    starts <- min(c(Time, (j + 1)))
                    stops  <- min(c(Time, (j + dl)))
                    events[i, starts:stops] <- 3
                    j       <- (j + dl)
                }else{
                    # No detention time
                    j <- j + 1
                }
            }else{
                # No event
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

 res <- simMoreCovs(N = 100, Time = 365, cov.val = c(0, 0), b = c(-7, .5, -.5) )
