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
\item{ion_concs}{See Usage.}
\item{P_K}{Defaults to \code{1}.}
\item{P_Na}{Defaults to \code{0.04}.}
\item{P_Cl}{Defaults to \code{0.45}.}
\item{T}{Defaults to \code{310.15}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{potential_mV}, \code{potential_V}, \code{numerator_mM}, \code{denominator_mM}, \code{slope_mV}, \code{permeabilities}, \code{T_K}, \code{units}, \code{method}.
}
\keyword{internal}
