% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwSqLap}
\alias{BwSqLap}
\title{Eq (3.136): 2N poles, half in the right half-plane, so this is NOT a}
\usage{
BwSqLap(s, Omega_c, N)
}
\arguments{
\item{s}{See Usage.}
\item{Omega_c}{See Usage.}
\item{N}{See Usage.}
}
\description{
filter until the N left-half-plane ones are selected by eq (3.138).
}
\value{
A list with \code{H}, \code{s}, \code{Omega_c}, \code{N}, \code{n_poles}, \code{half_are_right_half_plane}, \code{not_a_filter_until_the_poles_are_selected}, \code{method}.
}
\keyword{internal}
