% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwr_pivot_chol_rcond}
\alias{.gwr_pivot_chol_rcond}
\title{Pivoted Cholesky with a small ridge when the smallest pivot is}
\usage{
.gwr_pivot_chol_rcond(M)
}
\arguments{
\item{M}{See Usage.}
}
\description{
essentially zero; returns a list with the cholesky factor, the rank-deficient count, and the effective rank threshold.
}
\value{
A list with \code{L}, \code{rdef}, \code{piv}.
}
\keyword{internal}
