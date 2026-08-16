% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_omp}
\alias{.morie_bx_omp}
\title{Greedy atom picks with a least-squares reprojection on the support}
\usage{
.morie_bx_omp(x, D, sparsity, tol)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{D}{A matrix; indexed by row and column.}
\item{sparsity}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{tol}{Passed to \code{<=}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{DictCode}, \code{KsvdFit}, \code{OmpFit}. See the file header for the source the module follows.
}
\value{
A list with \code{coefficients}, \code{support}, \code{residual}.
}
\keyword{internal}
\examples{
set.seed(3)
D <- matrix(rnorm(40), 5, 8)
D <- D / sqrt(rowSums(D^2))
x <- D[2, ] * 1.5 - 0.7 * D[4, ]
.morie_bx_omp(x, D, sparsity = 2L, tol = 1e-8)
}
