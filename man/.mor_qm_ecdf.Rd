% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_qm_ecdf}
\alias{.mor_qm_ecdf}
\title{Piecewise-linear CDF through (x_(j), j/(n-1)), clamped to [0, 1]}
\usage{
.mor_qm_ecdf(sorted_x, v)
}
\arguments{
\item{sorted_x}{A vector; its length is taken and its elements indexed.}
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the qmDS_native implementation. Called by \code{morie_qmDS}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.mor_qm_ecdf(sorted_x = 5L, v = c(1, 2, 3, 4, 5, 6, 7, 8))
}
