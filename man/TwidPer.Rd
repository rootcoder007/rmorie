% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TwidPer}
\alias{TwidPer}
\title{Eq (3.89): indices reduce modulo N -- why the same roots of unity are}
\usage{
TwidPer(npoints, n, k) 
}
\arguments{
\item{npoints}{Coerced to integer by the body, with \code{as.integer}.}
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
\item{k}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
reused at every FFT stage, and why every DFT relation is periodic.
}
\value{
A list with \code{base}, \code{shift_k}, \code{shift_n}, \code{max_difference}, \code{holds}, \code{N}, \code{n}, \code{k}, \code{method}.
}
\keyword{internal}
\examples{
TwidPer(npoints = 5L, n = 5L, k = 5L)
}
