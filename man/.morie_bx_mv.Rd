% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_mv}
\alias{.morie_bx_mv}
\title{.morie_bx_mv}
\usage{
.morie_bx_mv(A, v)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{v}{A vector; its length is taken.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{.morie_bx_lstsq}, \code{.morie_bx_omp}, \code{BmiDec} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
.morie_bx_mv(matrix(c(1, 2, 3, 4), 2, 2), c(1, 1))
}
