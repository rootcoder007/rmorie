% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_mm}
\alias{.morie_bx_mm}
\title{Matrix product with compensated inner sums, not BLAS: the summation}
\usage{
.morie_bx_mm(A, B)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{B}{A matrix; indexed by row and column.}
}
\description{
order has to match the Python arm or an iterative update diverges
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_bx_mm(D, M)
}
