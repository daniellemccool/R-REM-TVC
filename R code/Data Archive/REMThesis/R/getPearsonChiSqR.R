#' Estimate pearson chi square residuals (in progress)
#'
#' This function isn't currently in use because I'm not sure whether or not this is a valid calculation.
#' @param fits Fits for the simulation study
#' @keywords residuals
#' @export
#' @examples
#' getPearsonChiSqR()

getPearsonChiSqR <- function(fits) {
    Y       <- rowSums(fits$events)
    x.hats  <- fits[2:4]
    bees    <- unlist(fits[5:7], recursive = FALSE)[c(1, 7, 13)]

    lambdas         <- mapply(function(x, b) exp(tensor(x, b, alongA = 3, alongB = 1)), x.hats, bees, SIMPLIFY = FALSE)
    Lambdas         <- lapply(lambdas, rowSums)

    p.resid <- lapply(Lambdas, function(X, Y) sum(((Y - X)^2)/X), Y = Y)

    return(p.resid)
}
