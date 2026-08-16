% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{kernel_smooth}
\alias{kernel_smooth}
\title{kernel_smooth}
\usage{
kernel_smooth(u, kernel = "epanechnikov")
}
\arguments{
\item{u}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{kernel}{One of \code{"epanechnikov"}, \code{"uniform"}. Defaults to \code{"epanechnikov"}.}
}
\description{
A step of the tlcvnp_native implementation. Called by \code{smoothed_parameter}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
kernel_smooth(u = 5L)
}
