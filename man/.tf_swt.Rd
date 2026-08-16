% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_swt}
\alias{.tf_swt}
\title{.tf_swt}
\usage{
.tf_swt(x, wavelet, levels)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{wavelet}{Passed to \code{.tf_filters}.}
\item{levels}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{Swt}, \code{WtVar}, \code{WtXcor}. See the file header for the source the module follows.
}
\value{
A list with \code{approx}, \code{details}, \code{approxes}.
}
\keyword{internal}
\examples{
set.seed(1)
x <- sin(2 * pi * (1:64) / 16) + rnorm(64, 0, 0.1)
.tf_swt(x, wavelet = "db4", levels = 2L)
}
