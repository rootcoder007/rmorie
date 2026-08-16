% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CwtRidge}
\alias{CwtRidge}
\title{CWT ridge detection of transient structures, Sec 8.8}
\usage{
CwtRidge(x, fs = 1, scales = NULL, wavelet = "mexh", w0 = 5, min_prominence = 0.1)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{scales}{Defaults to \code{NULL}.}
\item{wavelet}{The body requires: the signal has no wavelet energy at any scale. Defaults to \code{"mexh"}.}
\item{w0}{Defaults to \code{5}.}
\item{min_prominence}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{structures}, \code{n_structures}, \code{scalogram}, \code{scales}, \code{times}, \code{min_prominence}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CwtRidge(V)
}
