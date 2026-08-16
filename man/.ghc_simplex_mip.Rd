% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_simplex_mip}
\alias{.ghc_simplex_mip}
\title{.ghc_simplex_mip}
\usage{
.ghc_simplex_mip(A, b, c, tol = 1e-09, max_iter = 20000)
}
\description{
.ghc_simplex_mip
}
\value{
A list with \code{feasible}, \code{x}, \code{value}.
}
\keyword{internal}
\examples{
.ghc_simplex_mip(A = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8), c = c(1, 2, 3, 4, 5, 6, 7, 8))
}
