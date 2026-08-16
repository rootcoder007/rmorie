% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DiracDelta}
\alias{DiracDelta}
\title{Eq (3.24): delta(t) is undefined at t = 0 and 0 elsewhere.  A}
\usage{
DiracDelta(t, width = NULL)
}
\arguments{
\item{t}{See Usage.}
\item{width}{Defaults to \code{NULL}.}
}
\description{
generalized function has no pointwise value at the origin, so NA is returned there.  With width= the unit-area rectangle of Figure 3.10 is returned instead -- the approximating family, not the delta.
}
\value{
A list with \code{delta}, \code{t}, \code{width}, \code{height}, \code{undefined_at_zero}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DiracDelta(V)
}
