% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causrddc_global_derivative}
\alias{.causrddc_global_derivative}
\title{.causrddc_global_derivative}
\usage{
.causrddc_global_derivative(x, y, side, order, deriv)
}
\arguments{
\item{x}{A vector; indexed elementwise.}
\item{y}{A vector; indexed elementwise.}
\item{side}{Passed to \code{>}.}
\item{order}{Numeric; combined arithmetically in the body.}
\item{deriv}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the causrddc_native implementation. Called by \code{morie_causrddc_rd_bandwidth}. See the file header for the source the module follows.
}
\value{
A list with \code{deriv}, \code{sigma2}.
}
\keyword{internal}
\examples{
.causrddc_global_derivative(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), side = 5L, order = 5L, deriv = c(1, 2, 3, 4, 5, 6, 7, 8))
}
