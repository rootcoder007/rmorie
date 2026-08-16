% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwSqMag}
\alias{BwSqMag}
\title{Eq (3.135): monotonic in both bands, no ripple anywhere -- the}
\usage{
BwSqMag(Omega, Omega_c, N)
}
\arguments{
\item{Omega}{See Usage.}
\item{Omega_c}{See Usage.}
\item{N}{See Usage.}
}
\description{
defining Butterworth property.  At the cutoff the squared magnitude is exactly 1/2 FOR EVERY ORDER, so raising N steepens the transition without moving the half-power point.
}
\value{
A list with \code{squared_magnitude}, \code{magnitude}, \code{Omega}, \code{Omega_c}, \code{N}, \code{half_power_at_cutoff}, \code{monotonic}, \code{no_ripple}, \code{cutoff_is_half_power_for_every_order}, \code{method}.
}
\keyword{internal}
