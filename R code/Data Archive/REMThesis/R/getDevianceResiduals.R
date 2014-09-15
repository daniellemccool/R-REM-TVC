#' Calculate Deviance Residuals
#'
#' Calculates the Deviance of the residuals
#' @param fits
#' @keywords deviance, residuals
#' @export
#' @examples
#' getDevianceResiduals()

getDevianceResiduals <- function(fits) {
    Y       <- rowSums(fits$events)
    x.hats  <- fits[2:4]
    bees    <- unlist(fits[5:7], recursive = FALSE)[c(1, 7, 13)]


    lambdas <- mapply(function(x, b) exp(tensor(x, b, alongA = 3, alongB = 1)), x.hats, bees, SIMPLIFY = FALSE)
    Lambdas <- lapply(lambdas, rowSums)

    dev.resid   <- lapply(Lambdas, function(X, Y) sign(X*Y) * sqrt((Y*log(Y/X)-(Y - X))), Y = Y)

    return(dev.resid)
}