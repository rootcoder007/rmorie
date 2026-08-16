% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smoothed_parameter}
\alias{smoothed_parameter}
\title{smoothed_parameter}
\usage{
smoothed_parameter(X, x0, h, kernel = "epanechnikov")
}
\arguments{
\item{X}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{kernel}{Passed to \code{kernel_smooth}. Defaults to \code{"epanechnikov"}.}
}
\description{
A step of the tlcvnp_native implementation. Called by \code{cv_tmle_smoothed}, \code{select_bandwidth}. See the file header for the source the module follows.
}
\value{
A list with \code{psi_h}, \code{se}, \code{h}, \code{n}, \code{influence_curve}, \code{note}.
}
\keyword{internal}
\examples{
smoothed_parameter(X = c(1, 2, 3, 4, 5, 6, 7, 8), x0 = 5L, h = 0.5)
}
