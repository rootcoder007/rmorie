% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_baytsm_dlm_local_level}
\alias{morie_baytsm_dlm_local_level}
\alias{morie_baytsm}
\title{The first-order polynomial DLM, forward filter and}
\usage{
morie_baytsm_dlm_local_level(y, V = 1, W = 0.1, m0 = 0, C0 = 1e+06)
}
\description{
R arm of baytsm -- the first-order polynomial DLM, forward filter and Forecasting and Dynamic Models, 2nd ed., Springer, Ch. 2 and Sec. 4.8.
}
\value{
A list with components \code{n}, \code{estimate}, \code{smoothed}, \code{smoothed_var}, \code{filtered}, \code{filtered_var}, \code{forecast}, \code{forecast_var}, \code{adaptive_coefficient}, \code{forecast_error}, \code{loglik}, \code{signal_to_noise}, \code{V}, \code{W}, \code{method}, \code{note}, \code{A}.
}
\references{
retrospective smoother. West, M. & Harrison, J. (1997) Bayesian
}
\keyword{internal}
