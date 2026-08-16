% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causrddc_kernel_constants}
\alias{.causrddc_kernel_constants}
\title{.causrddc_kernel_constants}
\usage{
.causrddc_kernel_constants(p, q, kernel = "triangular", n_grid = 2001)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{q}{Numeric; combined arithmetically in the body.}
\item{kernel}{Passed to \code{.causrddc_kern}. Defaults to \code{"triangular"}.}
\item{n_grid}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2001}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{morie_causrddc_rd_bandwidth}. See the file header for the source the module follows.
}
\value{
A list with \code{G}, \code{th}, \code{P}.
}
\keyword{internal}
\examples{
.causrddc_kernel_constants(p = 0.5, q = 0.5)
}
