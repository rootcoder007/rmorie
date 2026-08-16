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
\item{C}{Numeric; combined arithmetically in the body.}
\item{C0}{Numeric; combined arithmetically in the body. Defaults to \code{.C0_FIT}.}
\item{N}{Numeric; passed to \code{sqrt}. Defaults to \code{273.87}.}
\item{method}{The body requires: co2RF: method must be 'ar6' or 'myhre1998'. Defaults to \code{"ar6"}.}
\item{erf_adjustment}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the co2RF_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{sarf}, \code{alpha_prime}, \code{method_used}, \code{C}, \code{C0}, \code{N}, \code{erf_adjustment}, \code{method}.
}
\keyword{internal}
