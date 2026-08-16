% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_topeigs}
\alias{.morie_spx_topeigs}
\title{Top-k eigenpairs of a SYMMETRIC matrix by power iteration + deflation}
\usage{
.morie_spx_topeigs(A, k, iters = 400L)
}
\arguments{
\item{A}{See Usage.}
\item{k}{See Usage.}
\item{iters}{Defaults to \code{400L}.}
}
\description{
The start vector is fixed and slightly non-uniform: an all-ones start is orthogonal to the leading eigenvector of some ordinary matrices and fails silently when it is.
}
\value{
A list with \code{values}, \code{vectors}.
}
\keyword{internal}
