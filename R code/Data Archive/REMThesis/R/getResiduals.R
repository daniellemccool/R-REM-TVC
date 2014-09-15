#' Estimate residuals (in progress)
#'
#' This function isn't currently in use because I'm not sure whether or not this is a valid calculation.
#' @param fits Fits for the simulation study
#' @keywords residuals
#' @export
#' @examples
#' getResiduals()

getResiduals <- function(fits) {
    Y <- rowSums(fits$events)
    x.hats <- fits[2:4]
    bees <- unlist(fits[5:7], recursive = FALSE)[c(1, 7, 13)]

    lambdas         <- mapply(function(x, b) exp(tensor(x, b, alongA = 3, alongB = 1)), x.hats, bees, SIMPLIFY = FALSE)
    Lambdas         <- lapply(lambdas, rowSums)
    raw.resid       <- lapply(Lambdas, function(Lambda, Y) Y - Lambda, Y)
    pearson.resid   <- mapply(function(r, L) r/sqrt(L), raw.resid, Lambdas)

    return(list(raw.resid, pearson.resid))
}
