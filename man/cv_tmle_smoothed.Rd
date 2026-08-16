% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cv_tmle_smoothed}
\alias{cv_tmle_smoothed}
\title{cv_tmle_smoothed}
\usage{
cv_tmle_smoothed(X, x0, bandwidths, kernel = "epanechnikov", V = 5L, seed = 0L)
}
\arguments{
\item{X}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x0}{Passed to \code{select_bandwidth}.}
\item{bandwidths}{Passed to \code{select_bandwidth}.}
\item{kernel}{Passed to \code{select_bandwidth}. Defaults to \code{"epanechnikov"}.}
\item{V}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.}
}
\description{
A step of the tlcvnp_native implementation. Called by \code{morie_tlcvnp}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{se}, \code{ci}, \code{bandwidths}, \code{fold_estimates}, \code{V}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
cv_tmle_smoothed(X = c(1, 2, 3, 4, 5, 6, 7, 8), x0 = 5L, bandwidths = c(1, 2, 3, 4, 5, 6, 7, 8))
}
