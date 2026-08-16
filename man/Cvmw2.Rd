% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cvmw2}
\alias{Cvmw2}
\title{Cramer-von Mises W^2 -- Problem 4.14, p. 150}
\usage{
Cvmw2(x, cdf)
}
\description{
Cramer-von Mises W^2 -- Problem 4.14, p. 150
}
\value{
A list with \code{statistic}, \code{nw2}, \code{z}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(2)
Cvmw2(rnorm(25), cdf = pnorm)
}
