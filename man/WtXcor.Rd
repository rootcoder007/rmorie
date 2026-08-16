% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WtXcor}
\alias{WtXcor}
\title{Scale-by-scale wavelet cross-correlation, Whitcher et al. (2000)}
\usage{
WtXcor(x, y, wavelet = "db4", levels = 3, max_lag = 0)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{y}{Passed to \code{.tf_need}.}
\item{wavelet}{Passed to \code{.tf_swt}. Defaults to \code{"db4"}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
\item{max_lag}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{correlations}, \code{best_lags}, \code{covariances}, \code{scales}, \code{overall_correlation}, \code{levels}, \code{max_lag}, \code{wavelet}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
WtXcor(V, V)
}
