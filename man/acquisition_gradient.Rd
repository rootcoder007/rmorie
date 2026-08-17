% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{acquisition_gradient}
\alias{acquisition_gradient}
\title{acquisition_gradient}
\usage{
acquisition_gradient(gmu, gsd, mu, sd, best, acq = "ei", kappa = 2, xi = 0)
}
\arguments{
\item{gmu}{A vector; its length is taken.}
\item{gsd}{Numeric; combined arithmetically in the body.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{sd}{Numeric; combined arithmetically in the body.}
\item{best}{Numeric; combined arithmetically in the body.}
\item{acq}{One of \code{"ei"}, \code{"lcb"}, \code{"pi"}. Defaults to \code{"ei"}.}
\item{kappa}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{xi}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{maximise_acquisition}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
r <- acquisition_gradient(gmu = c(0.2, -0.1), gsd = c(-0.05, 0.03),
                          mu = 1.2, sd = 0.4, best = 1.0, acq = "ei")
r
}
