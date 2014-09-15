#' Truncated Recurrent Event Model (log)Likelihood for Time-Varying Covariates
#'
#' The meat and bones of the operation, this is the log-likelihood for the truncated recurrent event model.
#' @param b Current iteration's choice of coefficient (from Optim)
#' @param x.arr 3-dimensional array of covariates, time-varying or otherwise, with dim[1] representing persons, dim[2] representing covariates and dim[3] representing time.
#' @param ev.mat Event matrix of ones and zeroes with N rows and t columns representing event occurrence (1) or non-occurrence (0) per day per person.
#' @param Time Not used, holdover from previous method.
#' @keywords likelihood
#' @export
#' @examples
#' rem.mvtc.lik()

rem.mtvc.lik <- function(b, ev.mat, x.arr, Time) {
    b <- b
    u.mat <- exp(tensor(x.arr, b, alongA = 3, alongB = 1))
    logl  <- sum(log(u.mat[ev.mat == 1])) -
             sum(u.mat[ev.mat != 3]) -
             sum(log(1 - exp(-rowSums(u.mat))))
    return(-logl)
}