% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{XCorr}
\alias{XCorr}
\title{Bsacorr}
\usage{
XCorr(x, y, maxlag = NULL, normalize = FALSE, biased = TRUE)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{maxlag}{Defaults to \code{NULL}.}
\item{normalize}{Defaults to \code{FALSE}.}
\item{biased}{Defaults to \code{TRUE}.}
}
\description{
Part of the rangayyan_ccf implementation; see the file header for the source it follows.
}
\value{
A list with \code{ccf}, \code{lags}, \code{peak}, \code{peak_lag}, \code{n}, \code{biased}, \code{normalized}, \code{positive_lag_means_y_trails_x}, \code{biased_keeps_nonnegative_definiteness}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
XCorr(V, V)
}
