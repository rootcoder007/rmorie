% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{select_bandwidth}
\alias{select_bandwidth}
\title{select_bandwidth}
\usage{
select_bandwidth(X, x0, bandwidths, kernel = "epanechnikov", criterion = "lepski", C = 1)
}
\arguments{
\item{X}{See Usage.}
\item{x0}{See Usage.}
\item{bandwidths}{See Usage.}
\item{kernel}{Defaults to \code{"epanechnikov"}.}
\item{criterion}{One of \code{"lepski"}, \code{"smallest_se"}. Defaults to \code{"lepski"}.}
\item{C}{Defaults to \code{1}.}
}
\description{
A step of the tlcvnp_native implementation. Called by \code{cv_tmle_smoothed}. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{fit}, \code{criterion}, \code{all}, \code{note}.
}
\keyword{internal}
\examples{
select_bandwidth(X = c(1, 2, 3, 4, 5, 6, 7, 8), x0 = 5L, bandwidths = c(1, 2, 3, 4, 5, 6, 7, 8))
}
