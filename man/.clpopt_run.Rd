% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clpopt_run}
\alias{.clpopt_run}
\title{Internal: pivot to optimality. Mirrors clpopt._run}
\usage{
.clpopt_run(T, basis, cols, rule, blocked, max_iter)
}
\arguments{
\item{T}{A matrix; indexed by row and column.}
\item{basis}{A vector; its length is taken and its elements indexed.}
\item{cols}{A vector; indexed elementwise.}
\item{rule}{Compared against \code{"bland"}.}
\item{blocked}{See Usage.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the clpopt_native implementation. Called by \code{simplex}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
.clpopt_run(T = c(1, 2, 3, 4, 5, 6, 7, 8), basis = c(1, 2, 3, 4, 5, 6, 7, 8), cols = 5L, rule = c(1, 2, 3, 4, 5, 6, 7, 8), blocked = c(1, 2, 3, 4, 5, 6, 7, 8), max_iter = c(1, 2, 3, 4, 5, 6, 7, 8))
}
