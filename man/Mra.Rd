% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mra}
\alias{Mra}
\title{Multiresolution analysis, eqs (8.111)-(8.114)}
\usage{
Mra(x, wavelet = "db4", levels = 3) 
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{wavelet}{Passed to \code{.tf_dwt}. Defaults to \code{"db4"}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{approximation}, \code{details}, \code{bands}, \code{reconstruction_error}, \code{energy_per_band}, \code{levels}, \code{wavelet}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
x <- sin(2 * pi * (1:128) / 16) + rnorm(128, 0, 0.1)
str(Mra(x, wavelet = "db4", levels = 3), max.level = 1)
}
