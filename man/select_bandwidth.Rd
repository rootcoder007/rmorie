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
\item{criterion}{Defaults to \code{"lepski"}.}
\item{C}{Defaults to \code{1}.}
}
\description{
Part of the tlcvnp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{h}, \code{fit}, \code{criterion}, \code{all}, \code{note}.
}
\keyword{internal}
