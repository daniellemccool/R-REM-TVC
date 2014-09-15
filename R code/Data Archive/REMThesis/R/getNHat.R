#' Population size estimation
#'
#' Generates estimate of population size as given by the Horvitz-Thompson estimation procedure
#' @param x.arr 3-dimensional array of covariates, time-varying or otherwise, with dim[1] representing persons, dim[2] representing covariates and dim[3] representing time.
#' @param ev.mat Matrix of ones and zeroes with N rows and t columns representing event occurrence (1) or non-occurrence (0) per day per person.
#' @param b Estimated beta coefficients from optim procedure.
#' @keywords nhat
#' @export
#' @examples
#' getNHat()

getNHat <- function(x.arr, ev.mat, b) {
    u.mat   <- exp(tensor(x.arr, b, alongA = 3, alongB = 1))
    Lambda  <- rowSums(u.mat)
    y       <- rowSums(ev.mat)
    # Point Estimate
    p0      <- exp(-Lambda)
    pCap    <- 1-p0
    Nhat    <- sum(1/pCap)

    # Variance
    probLamb    <- sum(exp(-Lambda)*((1-exp(-Lambda))^-2))
    secder      <- (sum(y*(Lambda^-2))-probLamb)^-1
    var         <- (probLamb^2)*secder + probLamb
    return(list(Nhat = Nhat, var = var))
}
