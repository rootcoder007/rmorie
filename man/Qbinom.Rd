% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qbinom}
\alias{Qbinom}
\title{Qbinom}
\usage{
Qbinom(p, size, prob)
}
\arguments{
\item{p}{Iterated over elementwise, with \code{vapply}.}
\item{size}{See Usage.}
\item{prob}{See Usage.}
}
\description{
A step of the dist_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
Qbinom(p = 0.5, size = 5L, prob = 0.5)
}
