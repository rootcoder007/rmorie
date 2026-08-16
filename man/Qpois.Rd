% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qpois}
\alias{Qpois}
\title{Smallest k with cdf(k) >= p, as R defines it}
\usage{
Qpois(p, lambda)
}
\arguments{
\item{p}{Iterated over elementwise, with \code{vapply}.}
\item{lambda}{Passed to \code{Ppois}.}
}
\description{
A step of the dist_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
Qpois(p = 0.5, lambda = 0.5)
}
