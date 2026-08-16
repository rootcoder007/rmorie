% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03crossprod}
\alias{.s03crossprod}
\title{.s03crossprod}
\usage{
.s03crossprod(A)
}
\arguments{
\item{A}{A matrix; passed to \code{t}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.btres_xtxinv}, \code{.ch_ols_se} and 7 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.s03matmul}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.s03crossprod(M)
}
