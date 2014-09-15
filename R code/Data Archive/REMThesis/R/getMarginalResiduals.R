#' Estimate marginal residuals (in progress)
#'
#' This function isn't currently in use because I'm not sure whether or not this is a valid calculation.
#' @param fits Fits for the simulation study
#' @keywords residuals
#' @export
#' @examples
#' getMarginalResiduals()

getMarginalResiduals <- function(fits) {
    k       <- 0:max(rowSums(fits$events))
    x.hats  <- fits[2:4]
    bees    <- unlist(fits[5:7], recursive = FALSE)[c(1, 7, 13)]
    observed <- c(0, tabulate(rowSums(fits$events), nbins = k[length(k)]))

    lambdas <- mapply(function(x, b) exp(tensor(x, b, alongA = 3, alongB = 1)), x.hats, bees, SIMPLIFY = FALSE)
    Lambdas <- lapply(lambdas, rowSums)

    dPOtr <- function(k, L){sum((L^k*exp(-L))/(factorial(k)*(1-exp(-L))))}
    estimated <- lapply(Lambdas, function(x) sapply(k, dPOtr, L = x) )
    raw.resid <- lapply(estimated, function(e, o) (o - e), o = observed)
    table <- cbind(observed, do.call(cbind, estimated))
    print(xtable(table))
    return(estimated)

}