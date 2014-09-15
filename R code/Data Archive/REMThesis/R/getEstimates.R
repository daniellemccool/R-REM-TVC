#' General Simulation Function
#'
#' Calls simCat functions to generate data, then method functions to fill in TVCs, then estimates using optim, then calls SaveData() to file it. Basically the whole simulation script for convenience.
#' @param N Number of people in the simulation. Defaults to 1000.
#' @param Time Total length of simulation in days. Defaults to 365 for one year.
#' @param beta True-values for parameter generation.
#' @param sim Number of simulation iterations. Defaults to 25 in case I accidentally run it.
#' @param method Use method one or two, where method 1 is random movement between locations and method 2 prefers remaining with less chance of switching.
#' @param seed Seed to use for deterministic reasons. Defaults to 4885 for my birthday.
#' @keywords script
#' @export
#' @examples
#' getEstimates()

getEstimates <- function(N = 1000, Time = 365, beta = c(-7, -.5, -.25, .25, .5, 0), sim = 25, method = 1, seed = 4885) {

    set.seed(seed)
    covs <- length(beta)
    m.fv <- matrix(0, sim, covs-1)
    m.nv <- matrix(0, sim, covs-1)
    m.pr <- matrix(0, sim, covs-1)
    nhat <- matrix(0, sim, 6)

    for(sim in 1:sim){

        if (method == 1) {
            a <- simCat(N, Time, beta)    #
        } else if (method == 2) {
            a <- simCat2(N, Time, beta)
        }

        events <- a$E
        x.arr  <- a$XT

        x.hat.fv <- makeFirstValXhat(x.arr)
        x.hat.nv <- makeNvcbXhat(x.arr, events)
        x.hat.pr <- makePropXhat(x.arr, events)

        out.fv <- optim(beta[-6], rem.mtvc.lik, x.arr = x.hat.fv[,,-6], ev.mat = events, Time = Time, method = "BFGS")
        out.nv <- optim(beta[-6], rem.mtvc.lik, x.arr = x.hat.nv[,,-6], ev.mat = events, Time = Time, method = "BFGS")
        out.pr <- optim(beta[-6], rem.mtvc.lik, x.arr = x.hat.pr[,,-6], ev.mat = events, Time = Time, method = "BFGS")


        m.fv[sim,] <- out.fv$par

        m.nv[sim,] <- out.nv$par

        m.pr[sim,] <- out.pr$par

        nhat[sim,] <- unlist(c(getNHat(x.hat.fv[,,-6], events, out.fv$par),
                        getNHat(x.hat.nv[,,-6], events, out.nv$par),
                        getNHat(x.hat.pr[,,-6], events, out.pr$par)))

    }

    SaveData(list(m.fv, m.nv, m.pr, nhat), method)


    return(list(m.fv, m.nv, m.pr, nhat))
}
