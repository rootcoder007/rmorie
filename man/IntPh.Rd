% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IntPh}
\alias{IntPh}
\title{Eq (3.118): a constant -pi/2, because 1/(jw) is a fixed quarter turn}
\usage{
IntPh(omega)
}
\arguments{
\item{omega}{See Usage.}
}
\description{
A constant phase is NOT a constant delay: the group delay is the derivative, which is zero, so the integrator delays nothing.
}
\value{
A list with \code{phase}, \code{omega}, \code{constant}, \code{group_delay}, \code{constant_phase_is_not_constant_delay}, \code{sign_flips_for_negative_omega}, \code{method}.
}
\keyword{internal}
