% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlcvnp}
\alias{morie_tlcvnp}
\title{morie_tlcvnp}
\usage{
morie_tlcvnp(X, x0, bandwidths, kernel = "epanechnikov", V = 5L, seed = 0L) 
}
\arguments{
\item{X}{Passed to \code{cv_tmle_smoothed}.}
\item{x0}{Passed to \code{cv_tmle_smoothed}.}
\item{bandwidths}{Passed to \code{cv_tmle_smoothed}.}
\item{kernel}{Passed to \code{cv_tmle_smoothed}. Defaults to \code{"epanechnikov"}.}
\item{V}{Passed to \code{cv_tmle_smoothed}. Defaults to \code{5L}.}
\item{seed}{Passed to \code{cv_tmle_smoothed}. Defaults to \code{0L}.}
}
\description{
A step of the tlcvnp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{cv_tmle_smoothed}.
}
\keyword{internal}
\examples{
morie_tlcvnp(X = c(1, 2, 3, 4, 5, 6, 7, 8), x0 = 5L, bandwidths = c(1, 2, 3, 4, 5, 6, 7, 8))
}
