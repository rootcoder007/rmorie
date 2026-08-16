% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03ridgesolve}
\alias{.s03ridgesolve}
\title{.s03ridgesolve}
\usage{
.s03ridgesolve(A, b, ridge = 1e-10)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{b}{Passed to \code{.s03cholsolve}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.btres_xtxinv}, \code{.cfa_em}, \code{.htprd_ridge_cv} and 25 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.s03cholsolve}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.s03ridgesolve(M, V)
}
