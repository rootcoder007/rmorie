% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mambss}
\alias{morie_mambss}
\title{morie_mambss}
\usage{
morie_mambss(X, A, W_B, W_C, W_delta, delta_bias = NULL, b_B = NULL, b_C = NULL, b_delta = 0, rule = "zoh", D_skip = NULL)
}
\arguments{
\item{X}{Passed to \code{selective_scan}.}
\item{A}{Passed to \code{selective_scan}.}
\item{W_B}{Passed to \code{selective_scan}.}
\item{W_C}{Passed to \code{selective_scan}.}
\item{W_delta}{Passed to \code{selective_scan}.}
\item{delta_bias}{Passed to \code{selective_scan}.}
\item{b_B}{Passed to \code{selective_scan}.}
\item{b_C}{Passed to \code{selective_scan}.}
\item{b_delta}{Passed to \code{selective_scan}. Defaults to \code{0}.}
\item{rule}{Passed to \code{selective_scan}. Defaults to \code{"zoh"}.}
\item{D_skip}{Passed to \code{selective_scan}.}
}
\description{
A step of the mambss_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{selective_scan}.
}
\keyword{internal}
