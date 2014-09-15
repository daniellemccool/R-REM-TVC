#' Calculate Root Mean Square Error
#'
#' Calculates the root mean square error
#' @param est The parameter value estimated
#' @param true The parameter given in the simulation study design.
#' @keywords RMSE
#' @export
#' @examples
#' getRMSE()

getRMSE <- function(est, true) {
    true.val <- matrix(true, nrow = nrow(est), ncol = ncol(est), byrow = TRUE)
    sqrt(colMeans((true.val-est)^2))
}
