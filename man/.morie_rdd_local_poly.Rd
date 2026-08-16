% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rdd_local_poly}
\alias{.morie_rdd_local_poly}
\title{Internal: weighted local polynomial fit at a point}
\usage{
.morie_rdd_local_poly(x, y, x0, h, p = 1L, kernel = "triangular")
}
\arguments{
\item{x,y}{The running variable and the outcome, one side of the   cutoff.}
\item{x0}{The point to fit at -- the cutoff.}
\item{h}{The bandwidth.}
\item{p}{Polynomial order.}
\item{kernel}{One of the names in \code{.morie_rdd_kernels}.}
}
\description{
Kernel-weighted least squares of \code{y} on powers of \code{x - x0}, with the sandwich variance the CCT bandwidth needs. Mirrors the Python arm\'s \code{_local_poly_fit} term for term, so the two produce the same coefficients and the same \code{V}.
}
\value{
A list with \code{beta} (coefficients, intercept first) and   \code{V} (heteroskedasticity-robust covariance).
}
\keyword{internal}
