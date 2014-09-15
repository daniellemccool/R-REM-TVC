#' Population Size Estimation for Simulation
#'
#' Takes the population size estimations from the individual simulation study iterations and produces a table for display. Called from DisplaySimNhats.R
#' @param nhats These are the nhats from the simulation study.
#' @param N Total number of simulation iterations.
#' @param xtable Whether or not results should be formatted with xtable. Defaults to FALSE.
#' @keywords output, simulation
#' @export
#' @examples
#' GetSimNhats()

GetSimNhats <- function(nhats, N, xtable = FALSE) {
    sim <- nrow(nhats)
    cond <- ncol(nhats)
    nhat <- colMeans(nhats[, seq(1, cond, by = 2)])
    bias <- nhat - N
    lower.CI <- nhats[, seq(1, cond, by = 2)] - 1.96*sqrt(nhats[, seq(2, cond, by = 2)])
    upper.CI <- nhats[, seq(1, cond, by = 2)] + 1.96*sqrt(nhats[, seq(2, cond, by = 2)])
    coverage <- lower.CI <= N & upper.CI >= N
    percent.cov <- colSums(coverage)/sim*100
    CI.width <- colMeans(upper.CI - lower.CI)
    col.names <- c("PE Method", "IS Method", "PR Method")
    results <- rbind(nhat, bias, percent.cov, CI.width)
    colnames(results) <- col.names

    if (xtable) {
        return(xtable(results))
        } else {
            return(results)
        }

}