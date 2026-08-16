% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03matvec}
\alias{.s03matvec}
\title{.s03matvec}
\usage{
.s03matvec(A, v)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{v}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.ch_ols_se}, \code{.jnt_lmm_ri} and 36 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.s03matvec(M, D)
}
