% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ghk}
\alias{Ghk}
\title{Ghk: Goldman-Hodgkin-Katz voltage equation for the resting potential}
\usage{
Ghk(ion_concs, P_K = 1, P_Na = 0.04, P_Cl = 0.45, T = 310.15)
}
\arguments{
\item{ion_concs}{A vector; indexed elementwise.}
\item{P_K}{Numeric; passed to \code{min}. Defaults to \code{1}.}
\item{P_Na}{Numeric; passed to \code{min}. Defaults to \code{0.04}.}
\item{P_Cl}{Numeric; passed to \code{min}. Defaults to \code{0.45}.}
\item{T}{Numeric; combined arithmetically in the body. Defaults to \code{310.15}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{potential_mV}, \code{potential_V}, \code{numerator_mM}, \code{denominator_mM}, \code{slope_mV}, \code{permeabilities}, \code{T_K}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
Ghk(ion_concs = list(K_in = 140, K_out = 5, Na_in = 12, Na_out = 145,
                     Cl_in = 4, Cl_out = 110))
}
