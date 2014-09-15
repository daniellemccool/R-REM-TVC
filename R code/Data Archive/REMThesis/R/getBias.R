#' Calculate bias statistic
#'
#' Calculates the difference between the estimated parameter and the true parameter for the simulation study.
#' @param est The parameter value estimated
#' @param true The parameter given in the simulation study design.
#' @keywords bias
#' @export
#' @examples
#' getBias()

getBias <- function(est, true) {
    true.val <- matrix(true, nrow = nrow(est), ncol = ncol(est), byrow = TRUE)
    bias <- colMeans(true.val - est)

    return(bias)
}
