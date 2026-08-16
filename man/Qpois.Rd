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
\item{p}{See Usage.}
\item{lambda}{See Usage.}
}
\description{
Part of the dist_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
Qpois(p = 0.5, lambda = 0.5)
}
