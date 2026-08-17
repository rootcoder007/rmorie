% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{s6_layer}
\alias{s6_layer}
\title{s6_layer}
\usage{
s6_layer(X, A, W_B, W_C, W_delta, ...)
}
\arguments{
\item{X}{Passed to \code{selective_scan}.}
\item{A}{Passed to \code{selective_scan}.}
\item{W_B}{Passed to \code{selective_scan}.}
\item{W_C}{Passed to \code{selective_scan}.}
\item{W_delta}{Passed to \code{selective_scan}.}
\item{...}{Passed through.}
}
\description{
A step of the mambss_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
}
\keyword{internal}
\examples{
set.seed(2)
L <- 5; D <- 2; N <- 3
X <- matrix(rnorm(L * D), L, D)
r <- s6_layer(X, A = matrix(-abs(rnorm(D * N)), D, N),
              W_B = matrix(rnorm(N * D, 0, 0.3), N, D),
              W_C = matrix(rnorm(N * D, 0, 0.3), N, D),
              W_delta = matrix(rnorm(D, 0, 0.3), 1, D))
str(r, max.level = 1)
}
