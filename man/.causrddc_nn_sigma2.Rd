% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causrddc_nn_sigma2}
\alias{.causrddc_nn_sigma2}
\title{.causrddc_nn_sigma2}
\usage{
.causrddc_nn_sigma2(x, y, J, side_of)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; indexed elementwise.}
\item{J}{Numeric; combined arithmetically in the body.}
\item{side_of}{Passed to \code{>}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{morie_causrddc}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.causrddc_nn_sigma2(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), J = 5L, side_of = c(1, 2, 3, 4, 5, 6, 7, 8))
}
