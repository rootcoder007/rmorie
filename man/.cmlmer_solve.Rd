% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cmlmer_solve}
\alias{.cmlmer_solve}
\title{.cmlmer_solve}
\usage{
.cmlmer_solve(L, b)
}
\arguments{
\item{L}{A matrix; indexed by row and column.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the cmlmer_native implementation. Called by \code{.cmlmer_reml_at}, \code{morie_cmlmer_compressed_lmm}. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.cmlmer_solve(M, V)
}
