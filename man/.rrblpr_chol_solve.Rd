% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rrblpr_chol_solve}
\alias{.rrblpr_chol_solve}
\title{.rrblpr_chol_solve}
\usage{
.rrblpr_chol_solve(L, b)
}
\arguments{
\item{L}{A matrix; indexed by row and column.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the rrblpr_native implementation. Called by \code{.rrblpr_reml_at}, \code{morie_rrblpr_rr_blup}. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.rrblpr_chol_solve(M, V)
}
