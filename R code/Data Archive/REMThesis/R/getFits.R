#' Estimate fits for empirical example
#'
#' Estimates the fits for the empirical example
#' @param seed Starting seed
#' @param beta Beginning true values
#' @param weather Whether or not to include weather for the year in the calculations. Defaults to FALSE.
#' @keywords simulation, estimation
#' @export
#' @examples
#' getFits()

getFits <- function(seed, beta, weather = FALSE) {
    set.seed(seed)
    data    <- giveMeDataWithoutDetentions(weather = weather)
    events  <- data$events
    x.arr   <- data$X.arr

    Time    <- dim(x.arr)[2]

    x.hat.fv <- makeFirstValXhat(x.arr, 11:14)
    x.hat.nv <- makeNvcbXhat(x.arr, events)
    x.hat.pr <- makePropXhat(x.arr, events)

    out.fv <- optim(beta, rem.mtvc.lik, x.arr = x.hat.fv, ev.mat = events, Time = Time, method = "BFGS", hessian = TRUE)
    out.nv <- optim(beta, rem.mtvc.lik, x.arr = x.hat.nv, ev.mat = events, Time = Time, method = "BFGS", hessian = TRUE)
    out.pr <- optim(beta, rem.mtvc.lik, x.arr = x.hat.pr, ev.mat = events, Time = Time, method = "BFGS", hessian = TRUE)

    return(list(events = events, x.hat.fv = x.hat.fv, x.hat.nv = x.hat.nv, x.hat.pr = x.hat.pr, out.fv = out.fv, out.nv = out.nv, out.pr = out.pr))
}