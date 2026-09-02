% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{GrpDelay}
\alias{GrpDelay}
\title{Computed from the COEFFICIENTS, not by differentiating a numerical}
\usage{
GrpDelay(b, a = NULL, fs = 1000, n_freqs = 512) 
}
\arguments{
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{a}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1000}.}
\item{n_freqs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{512}.}
}
\description{
phase.  Differentiating is wrong at any zero on the unit circle, where the phase steps by PI -- a real step, not a branch artifact, so unwrapping (which removes multiples of 2 pi) leaves it and the derivative spikes.  A three-point mean has such a zero at w = 2pi/3 and its phase-differentiated delay comes out near 0 instead of 1.
}
\value{
A list with \code{f}, \code{group_delay}, \code{fs}, \code{mean}, \code{max_deviation}, \code{approximately_constant}, \code{defined}, \code{n_undefined}, \code{from_the_coefficients}, \code{phase_differentiation_breaks_at_unit_circle_zeros}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
GrpDelay(V)
}
