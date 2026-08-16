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
\item{X}{See Usage.}
\item{x0}{See Usage.}
\item{bandwidths}{See Usage.}
\item{kernel}{Defaults to \code{"epanechnikov"}.}
\item{V}{Defaults to \code{5L}.}
\item{seed}{Defaults to \code{0L}.}
}
\description{
Part of the tlcvnp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{se}, \code{ci}, \code{bandwidths}, \code{fold_estimates}, \code{V}, \code{method}, \code{note}.
}
\keyword{internal}
