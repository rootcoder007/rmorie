% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clpopt_pivot}
\alias{.clpopt_pivot}
\title{Internal: pivot on T[row, col]. Mirrors clpopt._pivot}
\usage{
.clpopt_pivot(T, row, col)
}
\arguments{
\item{T}{A matrix; indexed by row and column.}
\item{row}{Passed to \code{==}.}
\item{col}{See Usage.}
}
\description{
A step of the clpopt_native implementation. Called by \code{.clpopt_run}, \code{simplex}. See the file header for the source the module follows.
}
\value{
The value of \code{T}, as built in the body.
}
\keyword{internal}
