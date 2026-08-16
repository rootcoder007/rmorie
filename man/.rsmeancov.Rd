% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsmeancov}
\alias{.rsmeancov}
\title{Mean vector and covariance matrix (divisor |idx| - 1) of a subset}
\usage{
.rsmeancov(X, idx)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{idx}{A vector; its length is taken.}
}
\description{
A step of the helpers_rouss implementation. Called by \code{.rscstep}, \code{Fastm}, \code{Mcdcv} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{mu}, \code{S}.
}
\keyword{internal}
