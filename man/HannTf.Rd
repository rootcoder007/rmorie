% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannTf}
\alias{HannTf}
\title{Eq (3.103): (1/4)(1 + z^-1)^2, a DOUBLE zero at z = -1, that is at}
\usage{
HannTf(z)
}
\arguments{
\item{z}{See Usage.}
}
\description{
Nyquist.  The double zero is why the response reaches nought there smoothly and why the attenuation nearby is second order.
}
\value{
A list with \code{H}, \code{z}, \code{zeros}, \code{zero_multiplicity}, \code{zeros_at_nyquist}, \code{dc_gain}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HannTf(V)
}
