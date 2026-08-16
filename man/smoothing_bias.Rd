% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smoothing_bias}
\alias{smoothing_bias}
\title{smoothing_bias}
\usage{
smoothing_bias(true_density, x0, h, smoothness = 2)
}
\arguments{
\item{true_density}{See Usage.}
\item{x0}{See Usage.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{smoothness}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2}.}
}
\description{
A step of the tlcvnp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{bias_order}, \code{h}, \code{smoothness}, \code{note}.
}
\keyword{internal}
\examples{
smoothing_bias(true_density = c(1, 2, 3, 4, 5, 6, 7, 8), x0 = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
