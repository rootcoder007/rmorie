% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qbeta}
\alias{Qbeta}
\title{Qbeta}
\usage{
Qbeta(p, shape1, shape2)
}
\arguments{
\item{p}{See Usage.}
\item{shape1}{See Usage.}
\item{shape2}{See Usage.}
}
\description{
A step of the dist_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
Qbeta(p = 1, shape1 = c(1, 2, 3, 4, 5, 6, 7, 8), shape2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
