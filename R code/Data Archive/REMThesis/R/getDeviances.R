#' Calculate Deviances
#'
#' Calculates deviances
#' @param fits
#' @keywords deviances
#' @export
#' @examples
#' getDeviances()

getDeviances <- function(fits) {
    Y       <- rowSums(fits$events)
    x.hats  <- fits[2:4]
    bees    <- unlist(fits[5:7], recursive = FALSE)[c(1, 7, 13)]

    lambdas         <- mapply(function(x, b) exp(tensor(x, b, alongA = 3, alongB = 1)), x.hats, bees, SIMPLIFY = FALSE)
    Lambdas         <- lapply(lambdas, rowSums)

    Deviances <- lapply(Lambdas, function(X, Y) 2*sum(Y*log(Y/X)-(Y - X)), Y = Y)

    return(Deviances)
}
