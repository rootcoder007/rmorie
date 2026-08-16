% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{acquire}
\alias{acquire}
\title{acquire}
\usage{
acquire(mu, sd, best, acq = "ei", kappa = 2, xi = 0)
}
\arguments{
\item{mu}{See Usage.}
\item{sd}{See Usage.}
\item{best}{See Usage.}
\item{acq}{One of \code{"ei"}, \code{"lcb"}, \code{"pi"}. Defaults to \code{"ei"}.}
\item{kappa}{Defaults to \code{2}.}
\item{xi}{Defaults to \code{0}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{bayopt}, \code{maximise_acquisition}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
acquire(mu = c(1, 2, 3, 4, 5, 6, 7, 8), sd = 5L, best = c(1, 2, 3, 4, 5, 6, 7, 8))
}
