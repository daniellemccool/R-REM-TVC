% Generated by roxygen2 (4.0.2.9000): do not edit by hand
\name{rem.mtvc.lik}
\alias{rem.mtvc.lik}
\title{Truncated Recurrent Event Model (log)Likelihood for Time-Varying Covariates}
\usage{
rem.mtvc.lik(b, ev.mat, x.arr, Time)
}
\arguments{
\item{b}{Current iteration's choice of coefficient (from Optim)}

\item{ev.mat}{Event matrix of ones and zeroes with N rows and t columns representing event occurrence (1) or non-occurrence (0) per day per person.}

\item{x.arr}{3-dimensional array of covariates, time-varying or otherwise, with dim[1] representing persons, dim[2] representing covariates and dim[3] representing time.}

\item{Time}{Not used, holdover from previous method.}
}
\description{
The meat and bones of the operation, this is the log-likelihood for the truncated recurrent event model.
}
\examples{
rem.mvtc.lik()
}
\keyword{likelihood}

