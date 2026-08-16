% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Nernst}
\alias{Nernst}
\title{Nernst: equilibrium (reverse) potential of a single ionic species}
\usage{
Nernst(T = 310.15, z = 1, conc_out = 5, conc_in = 140, ion = "K+")
}
\arguments{
\item{T}{Defaults to \code{310.15}.}
\item{z}{Defaults to \code{1}.}
\item{conc_out}{Defaults to \code{5}.}
\item{conc_in}{Defaults to \code{140}.}
\item{ion}{Defaults to \code{"K+"}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{ion}, \code{potential_mV}, \code{potential_V}, \code{slope_mV}, \code{ratio}, \code{T_K}, \code{z}, \code{conc_out_mM}, \code{conc_in_mM}, \code{units}, \code{method}.
}
\keyword{internal}
