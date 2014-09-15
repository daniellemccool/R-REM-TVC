#' Estimate pearson Goodness of Fit statistic (in progress)
#'
#' This function isn't currently in use because I'm not sure whether or not this is a valid calculation.
#' @param fits Fits for the simulation study
#' @keywords residuals
#' @export
#' @examples
#' getPearsonGF()

getPearsonGF <- function(fits) {
    Y       <- rowSums(fits$events)
    x.hats  <- fits[2:4]
    bees    <- unlist(fits[5:7], recursive = FALSE)[c(1, 7, 13)]

    lambdas         <- mapply(function(x, b) exp(tensor(x, b, alongA = 3, alongB = 1)), x.hats, bees, SIMPLIFY = FALSE)
    Lambdas         <- lapply(lambdas, rowSums)

    GoF <- lapply(Lambdas, function(X, Y) sum((Y - X)/sqrt(X)), Y = Y)
    return(GoF)
}