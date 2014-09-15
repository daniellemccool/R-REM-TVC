#' Simulation Method 1 (Random movement)
#'
#' Time-varying categorical covariate representing where a person is on any given day. They have two primary locations - home and work with .65 and .3 probability respectively. There is also .05 probability to be randomly in a different city on any given day.
#' @param N Total number of people
#' @param Time Total length of simulation in days
#' @param b Covariate values for simulating risk of event.
#' @keywords simulation
#' @export
#' @examples
#' simCat()

simCat <- function(N,Time,b) {

    n.categories <- length(b)

    # Equal probability for being in any "city" for both
    # work and home.
    home <- cbind(1, t(rmultinom(N, 1, c(rep(1, n.categories-1)))))
    work <- cbind(1, t(rmultinom(N, 1, c(rep(1, n.categories-1)))))

    # The matrix where represents where a person is at
    # any given moment.
    where <- matrix(0, ncol = ncol(home), nrow = nrow(home))

    # We create random uniform numbers between 0 and 1
    # to determine the probability of where people will
    # be at any given time. If goes.where is 0, they are
    # at work. If goes.where is 1, they will go home
    # and if goes.where is 2, they will go to a random
    # location.

    changes.location <- matrix(runif(N*Time), nrow = N, ncol = Time)
    goes.where <- matrix(0, nrow = N, ncol = Time)
    goes.where[changes.location>.3] <- 1
    goes.where[changes.location>.95] <- 2



    # We need to know where they are on the first day
    # in order to have a starting rate for the event
    # probability.
    for(i in 1:N){
      if(goes.where[i, 1] == 1){where[i,] <- home[i,]} # If at home on day 1
      else if(goes.where[i, 1] == 0){where[i,] <- work[i,]}
      else if(goes.where[i, 1] == 2){where[i,] <- cbind(1, t(rmultinom(1, 1, c(rep(1, n.categories-1)))))}
    }

    # XT is where we will record the information for
    # people who experience an event.
    XT <- array(0, c(N, Time, (n.categories)))

    # For the time being, we have XT.check to capture
    # the full record (that we would ordinarily not
    # know) just to ensure our models are okay.
    XT.check <- array(0, c(N, Time, (n.categories)))

    # This records events per day. 1 is an event, 0
    # is no event, therefore E is an NxTime matrix.
    E    <- matrix(0, N, Time)

    # This is the beginning rate, a N-length vector.
    rate  <- exp(where%*%b)

    for(i in 1:N){
        for(j in 1:Time){
            if(runif(1) < rate[i]){
                E[i, j]     <- 1
                XT[i, j, ]  <- where[i, ]
            }

            # Just for checking, records the full history.
            XT.check[i, j, ] <- where[i, ]

            # Where are they for the next day?
            if(goes.where[i, j] == 1){where[i,]      <- home[i,]}
            else if(goes.where[i, j] == 0){where[i,] <- work[i,]}
            else if(goes.where[i, j] == 2){where[i,] <- cbind(1, t(rmultinom(1, 1, c(rep(1, n.categories-1)))))}

            # New rate for the new place.
            rate[i] <- exp(where[i,]%*%b)

        } # End Time loop
    } # End N loop

    NE       <- matrix(rowSums(E),ncol=1)   # number of events
    XT       <- XT[NE>0,,]           # select XT, events > 0
    XT.check <- XT.check[NE>0,,]
    E        <- E[NE>0,]            # select events, events > 0
    NE       <- NE[NE>0,]


    list(XT=XT,E=E,NE=NE, XT.check = XT.check)
}
