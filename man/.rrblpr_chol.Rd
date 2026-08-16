% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rrblpr_chol}
\alias{.rrblpr_chol}
\title{Cholesky factor, lower triangular, with a scaled jitter}
\usage{
.rrblpr_chol(A)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
}
\description{
A step of the rrblpr_native implementation. Called by \code{.rrblpr_reml_at}, \code{morie_rrblpr_rr_blup}. See the file header for the source the module follows.
}
\value{
The value of \code{L}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.rrblpr_chol(M)
}
