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
\item{mu}{See Usage.}
\item{sd}{See Usage.}
\item{kappa}{Defaults to \code{2}.}
}
\description{
Part of the bayopt_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
lower_confidence_bound(V, V)
}
