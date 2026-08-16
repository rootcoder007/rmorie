% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causrddc_solve}
\alias{.causrddc_solve}
\title{.causrddc_solve}
\usage{
.causrddc_solve(M, b)
}
\arguments{
\item{M}{A matrix; passed to \code{solve}.}
\item{b}{A matrix; passed to \code{solve}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{.causrddc_global_derivative}, \code{.causrddc_hc_sigma2}, \code{.causrddc_local_poly_weights} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
.causrddc_solve(M = 5L, b = 5L)
}
