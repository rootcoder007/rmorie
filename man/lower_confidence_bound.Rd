% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lower_confidence_bound}
\alias{lower_confidence_bound}
\title{lower_confidence_bound}
\usage{
lower_confidence_bound(mu, sd, kappa = 2) 
}
\arguments{
\item{mu}{Numeric; combined arithmetically in the body.}
\item{sd}{Numeric; combined arithmetically in the body.}
\item{kappa}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{acquire}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
lower_confidence_bound(V, V)
}
