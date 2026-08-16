% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_solve}
\alias{.sgflrt_solve}
\title{.sgflrt_solve}
\usage{
.sgflrt_solve(L, b)
}
\arguments{
\item{L}{A matrix; indexed by row and column.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the sgflrt_native implementation. Called by \code{.sgflrt_inv}, \code{.sgflrt_laplace}, \code{morie_sgflrt_spatial_glmm_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
