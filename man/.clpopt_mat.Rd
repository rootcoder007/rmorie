% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clpopt_mat}
\alias{.clpopt_mat}
\title{Internal: build a numeric matrix from a list-of-lists / matrix}
\usage{
.clpopt_mat(A, name, ncol = NULL)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{name}{See Usage.}
\item{ncol}{Defaults to \code{NULL}.}
}
\description{
Mirrors clpopt._mat.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.clpopt_mat(V, V)
}
