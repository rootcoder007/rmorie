% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{radiative_forcing_co2}
\alias{radiative_forcing_co2}
\title{radiative_forcing_co2}
\usage{
radiative_forcing_co2(C, C0 = .C0_FIT, N = 273.87, method = "ar6", erf_adjustment = FALSE)
}
\arguments{
\item{C}{See Usage.}
\item{C0}{Defaults to \code{.C0_FIT}.}
\item{N}{Defaults to \code{273.87}.}
\item{method}{Defaults to \code{"ar6"}.}
\item{erf_adjustment}{Defaults to \code{FALSE}.}
}
\description{
Part of the co2RF_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{sarf}, \code{alpha_prime}, \code{method_used}, \code{C}, \code{C0}, \code{N}, \code{erf_adjustment}, \code{method}.
}
\keyword{internal}
