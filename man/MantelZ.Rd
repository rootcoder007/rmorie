% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MantelZ}
\alias{MantelZ}
\title{Gaussian Z-test of Sec. 1.3.1 with U of eq (1.10).  The book states}
\usage{
MantelZ(coords, x, w, u = NULL) 
}
\arguments{
\item{coords}{Accepted by the signature and not used anywhere in the body.}
\item{x}{Passed to \code{.morie_spx_chkv}.}
\item{w}{Passed to \code{.morie_spx_chkw}.}
\item{u}{Optional; may be \code{NULL}. Passed to \code{.morie_spx_chkw}.}
}
\description{
the approach but does not print Eg[M2] or Varg[M2]; both are derived from the quadratic-form moments and are stated in the Python docstring.  Only the SYMMETRIC part of W contributes.
}
\value{
A list with \code{m2}, \code{expectation}, \code{variance}, \code{z}, \code{p_value}, \code{sigma2}, \code{n}, \code{gaussian_moments_apply}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
coords <- matrix(runif(10), 5, 2)
W <- matrix(0, 5, 5)
W[abs(row(W) - col(W)) == 1] <- 1
MantelZ(coords, rnorm(5), W)
}
