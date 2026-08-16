% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03matmul}
\alias{.s03matmul}
\title{.s03matmul}
\usage{
.s03matmul(A, B)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{B}{A matrix; indexed by row and column.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.s03crossprod}, \code{Fevdc}, \code{Fnlm} and 11 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.s03matmul(M, M)
}
