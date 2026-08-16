% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{expected_improvement}
\alias{expected_improvement}
\title{expected_improvement}
\usage{
expected_improvement(mu, sd, best, xi = 0)
}
\arguments{
\item{mu}{Numeric; combined arithmetically in the body.}
\item{sd}{Numeric; combined arithmetically in the body.}
\item{best}{Numeric; combined arithmetically in the body.}
\item{xi}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{acquire}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
expected_improvement(mu = c(1, 2, 3, 4, 5, 6, 7, 8), sd = 5L, best = c(1, 2, 3, 4, 5, 6, 7, 8))
}
