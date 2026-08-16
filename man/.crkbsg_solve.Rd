% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.crkbsg_solve}
\alias{.crkbsg_solve}
\title{Gaussian elimination with partial pivoting. The cokriging matrix is}
\usage{
.crkbsg_solve(A, b)
}
\arguments{
\item{A}{See Usage.}
\item{b}{A vector; its length is taken.}
}
\description{
symmetric but NOT positive definite -- the Lagrange rows see to that -- so a Cholesky solve is not available.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
