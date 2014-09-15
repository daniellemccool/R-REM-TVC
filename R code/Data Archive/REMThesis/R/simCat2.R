#' Simulation Method 2 (Structured movement)
#'
#' # Time-varying categorical covariate representing a value that has a 25% chance to change to a "safer" region, but only when there is an event.
#' @param N Total number of people
#' @param Time Total length of simulation in days
#' @param b Covariate values for simulating risk of event.
#' @keywords simulation
#' @export
#' @examples
#' simCat2()

simCat2 <- function(N,Time,b) {

    n.categories <- length(b)

    home <- cbind(1, t(rmultinom(N, 1, c(rep(1, n.categories-1)))))

    # XT is where we will record the information for
    # people who experience an event.
    XT <- array(0, c(N, Time, (n.categories)))


    # This records events per day. 1 is an event, 0
    # is no event, therefore E is an NxTime matrix.
    E    <- matrix(0, N, Time)

    # This is the beginning rate, a N-length vector.
    rate  <- exp(home%*%b)

    for(i in 1:N){
        for(j in 1:Time){
            if(runif(1) < rate[i]){
                E[i, j]     <- 1
                XT[i, j, ]  <- home[i, ]
                if(runif(1) < .25){
                    if(!all(home[i,] == c(1, 1, 0, 0, 0, 0))){
                        new.val <- Lag(home[i, ], -1)
                        home[i, ] <- c(1, new.val[-6])
                    }
                }
            }

            rate[i] <- exp(home[i,]%*%b)

        } # End Time loop
    } # End N loop

    NE       <- matrix(rowSums(E),ncol=1)   # number of events
    XT       <- XT[NE>0,,]           # select XT, events > 0
    E        <- E[NE>0,]            # select events, events > 0
    NE       <- NE[NE>0,]


    list(XT=XT,E=E,NE=NE)
}
