% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EegAcf}
\alias{EegAcf}
\title{Phi(m) = (1/N) sum_n x(n) x(n + m).  BOTH the raw phi and the}
\usage{
EegAcf(x, fs, maxlag = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{maxlag}{Defaults to \code{NULL}.}
}
\description{
normalized rho are returned: rho is what a threshold can be set on, phi is what the equation says, and conflating them makes any threshold depend on the signal amplitude.
}
\value{
A list with \code{acf}, \code{normalized}, \code{lags}, \code{fs}, \code{peak_lag}, \code{peak_lag_seconds}, \code{implied_frequency_hz}, \code{peak_value}, \code{a_rhythm_gives_a_periodic_acf}, \code{robust_to_amplitude_variation}, \code{method}.
}
\keyword{internal}
