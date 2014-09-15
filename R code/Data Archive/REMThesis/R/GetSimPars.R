#' Beta Coefficient Estimates for Simulation
#'
#' Displays the beta estimates for the simulation study.
#' @param pars Parameter (Beta) estimates from simulation study
#' @param true.value The true value of the parameters that were estimated in the sim study.
#' @param xtable Whether or not results should be formatted with xtable. Defaults to FALSE.
#' @keywords output, simulation
#' @export
#' @examples
#' GetSimPars()

GetSimPars <- function(pars, true.value, xtable = FALSE) {

    col.names <- c("PE Method", "IS Method", "PR Method")
    row.names <- c("B0", "B1", "B2", "B3", "B4")
    par.ests <- matrix(colMeans(pars), nrow = 5, ncol = 3, dimnames = list(row.names, col.names))
    raw.bias <- matrix(getBias(pars, true.value), nrow = 5, ncol = 3, dimnames = list(row.names, col.names))
    rel.bias <- raw.bias/true.value
    par.rmse <- matrix(getRMSE(pars, true.value), nrow = 5, ncol = 3, dimnames = list(row.names, col.names))

    if (xtable == TRUE)
    {
        return(xtable(cbind(true.value, par.ests, rel.bias, par.rmse)))
    } else {
        return(list(true.value = true.value, par.ests = par.ests, raw.bias = raw.bias, rel.bias = rel.bias, par.rmse = par.rmse))
    }

}