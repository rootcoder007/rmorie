% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causrddc_kern}
\alias{.causrddc_kern}
\title{.causrddc_kern}
\usage{
.causrddc_kern(u, kernel)
}
\arguments{
\item{u}{Numeric; passed to \code{abs}.}
\item{kernel}{One of \code{"triangular"}, \code{"uniform"}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{.causrddc_hc_sigma2}, \code{.causrddc_kernel_constants}, \code{.causrddc_local_poly_weights}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.causrddc_kern(u = 5L, kernel = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
