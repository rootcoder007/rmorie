% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_chol}
\alias{.sgflrt_chol}
\title{.sgflrt_chol}
\usage{
.sgflrt_chol(A, rel_jitter = 1e-10)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{rel_jitter}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
}
\description{
A step of the sgflrt_native implementation. Called by \code{.sgflrt_laplace}, \code{morie_sgflrt_spatial_glmm_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{L}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.sgflrt_chol(M)
}
