% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nashq_solve}
\alias{.nashq_solve}
\title{.nashq_solve}
\usage{
.nashq_solve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{b}{See Usage.}
}
\description{
A step of the nashq_native implementation. Called by \code{.nashq_indifference}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.nashq_solve(M, V)
}
