#' Table of Population Size Estimates for Empirical Example
#'
#' Generates a table of the population size estimates from the different methods on the empirical example.
#' @param fits.w The fits from the empirical example including weather for the year.
#' @param xtable Whether or not results should be formatted with xtable. Defaults to FALSE.
#' @keywords output, empirical
#' @export
#' @examples
#' NHatTable()

NHatTable <- function(fits.w, xtable = FALSE) {
    events <- fits.w$events
    b.fv <- fits.w$out.fv$par
    b.nv <- fits.w$out.nv$par
    b.pr <- fits.w$out.pr$par

    x.hat.fv <- fits.w$x.hat.fv
    x.hat.nv <- fits.w$x.hat.nv
    x.hat.pr <- fits.w$x.hat.pr

    nhats <- rbind(getNHat(x.hat.fv, events, fits.w$out.fv$par), getNHat(x.hat.nv, events, fits.w$out.nv$par), getNHat(x.hat.pr, events, fits.w$out.pr$par))
    nhats <- matrix(unlist(nhats), nrow = 3)
    lower.ci <- nhats[, 1] - 1.96*sqrt(nhats[, 2])
    upper.ci <- nhats[, 1] + 1.96*sqrt(nhats[, 2])

    res.tab <- cbind(nhats[, 1], lower.ci, upper.ci)
    colnames(res.tab) <- c("Nhat", "Lower CI", "Upper CI")
    rownames(res.tab) <- c("PE Method", "IS Method", "PR Method")
    if(xtable == TRUE){
        return(xtable(res.tab))
    }else{
        return(print(res.tab))
    }
}