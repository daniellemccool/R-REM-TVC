#' Proportional Representation Method
#'
#' This is the function for the method deemed "Proportional Representation".  Takes the N by Covariate by Time array and an index of which columns vary over time and produces an array with the missing time-varying covariates filled in sampling from all possible values obtained.
#' @param x.arr 3-dimensional array of covariates, time-varying or otherwise, with dim[1] representing persons, dim[2] representing covariates and dim[3] representing time.
#' @param ev Event matrix of ones and zeroes with N rows and t columns representing event occurrence (1) or non-occurrence (0) per day per person.
#' @keywords method, tvc
#' @export
#' @examples
#' makePropXhat()

makePropXhat <- function(x.arr, ev) {
    N <- nrow(x.arr)
    x.arr.p <- aperm(x.arr, perm = c(1, 3, 2))
    x.hat <- array(0, dim = dim(x.arr.p))

    for (i in 1:N) {
        days <- which(ev[i,] == 1)
        events <- length(days)
        if(events > 1){
            x <- sample(days, 365, replace = TRUE)
            x[days] <- days
        }else{
            x <- days
        }
        mat <- x.arr.p[i, , x]
        x.hat[i, , 1:365] <- mat
    }
    x.hat <- aperm(x.hat, perm = c(1, 3, 2))
    return(x.hat)
}