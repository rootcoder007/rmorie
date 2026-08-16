% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.safrl_mat}
\alias{.safrl_mat}
\title{.safrl_mat}
\usage{
.safrl_mat(M, name)
}
\arguments{
\item{M}{A matrix; indexed by row and column.}
\item{name}{See Usage.}
}
\description{
A step of the safrl_native implementation. Called by \code{morie_safrl}. See the file header for the source the module follows.
}
\value{
The value of \code{rows}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.safrl_mat(M, V)
}
