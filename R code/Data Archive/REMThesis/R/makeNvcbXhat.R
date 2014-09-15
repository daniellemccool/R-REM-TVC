#' Interval-stable Method
#'
#' This is the function for the method deemed "Interval-Stable" method or "Next-value carried backwards." Takes the N by Covariate by Time array and an index of which columns vary over time and produces an array with the missing time-varying covariates filled in with the next value that is present chronologically. Fills in the last interval with the last value chronologically.
#' @param x.arr 3-dimensional array of covariates, time-varying or otherwise, with dim[1] representing persons, dim[2] representing covariates and dim[3] representing time.
#' @param ev Event matrix of ones and zeroes with N rows and t columns representing event occurrence (1) or non-occurrence (0) per day per person.
#' @keywords method, tvc
#' @export
#' @examples
#' makeNvcbXhat()

makeNvcbXhat <- function(x.arr, ev) {
    N <- nrow(x.arr)
    x.arr.p <- aperm(x.arr, perm = c(1, 3, 2))
    x.hat <- array(0, dim = dim(x.arr.p))

    for (i in 1:N) {
        days   <- which(ev[i,] == 1)
        events <- length(days)
        time   <- 0
        for (k in 1:events) {
            x.hat[i, ,time:days[k] ] <- x.arr.p[i, , days[k]]
            time <- days[k] + 1
        }
        x.hat[i, ,(time-1):365 ] <- x.arr.p[i, ,tail(days, n = 1) ]
    }
    x.hat <- aperm(x.hat, perm = c(1, 3, 2))

    return(x.hat)
}