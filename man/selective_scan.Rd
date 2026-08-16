% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{selective_scan}
\alias{selective_scan}
\title{selective_scan}
\usage{
selective_scan(X, A, W_B, W_C, W_delta, delta_bias = NULL, b_B = NULL, b_C = NULL, b_delta = 0, rule = "zoh", D_skip = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{A}{See Usage.}
\item{W_B}{See Usage.}
\item{W_C}{See Usage.}
\item{W_delta}{See Usage.}
\item{delta_bias}{Defaults to \code{NULL}.}
\item{b_B}{Defaults to \code{NULL}.}
\item{b_C}{Defaults to \code{NULL}.}
\item{b_delta}{Defaults to \code{0}.}
\item{rule}{Defaults to \code{"zoh"}.}
\item{D_skip}{Defaults to \code{NULL}.}
}
\description{
Part of the mambss_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{y}, \code{estimate}, \code{state}, \code{delta}, \code{L}, \code{D}, \code{N}, \code{rule}, \code{time_invariant}, \code{method}.
}
\keyword{internal}
