#' Poisson Equivalent Method
#'
#' This is the function for the method deemed Poisson-Equivalent or "First-Value". Takes the N by Covariate by Time array and an index of which columns vary over time and produces an array with the missing time-varying covariates filled in with the first value that occurs.
#' @param x.arr 3-dimensional array of covariates, time-varying or otherwise, with dim[1] representing persons, dim[2] representing covariates and dim[3] representing time.
#' @param tvc.cols Index of which columns vary over time.
#' @keywords method, tvc
#' @export
#' @examples
#' makeFirstValXhat()

makeFirstValXhat <- function(x.arr, tvc.cols) {
    N      <- nrow(x.arr)
    Time   <- dim(x.arr)[2]
    n.covs <- dim(x.arr)[3]

    day <- matrix(0, nrow = N, ncol = 1)
    x.0 <- matrix(0, nrow = N, ncol = dim(x.arr)[3])
    for (i in 1:N) {
        day <- arrayInd(which.min(!(x.arr[i,,tvc.cols] != 0)), .dim = dim(x.arr)[2])
        x.0[i,] <- x.arr[i, day, ]
    }
    x.hat <- array(x.0, dim = c(N, n.covs, Time))
    x.hat <- aperm(x.hat, perm = c(1, 3, 2))
    return(x.hat)
}