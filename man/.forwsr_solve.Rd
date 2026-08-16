% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.forwsr_solve}
\alias{.forwsr_solve}
\title{.forwsr_solve}
\usage{
.forwsr_solve(A, b)
}
\arguments{
\item{A}{Passed to \code{cbind}.}
\item{b}{A vector; its length is taken.}
}
\description{
A step of the forwsr_native implementation. Called by \code{morie_forwsr_ols_fit}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.forwsr_solve(M, D)
}
