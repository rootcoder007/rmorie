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
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{A}{A matrix; passed to \code{as.matrix}.}
\item{W_B}{A matrix; passed to \code{as.matrix}.}
\item{W_C}{A matrix; passed to \code{as.matrix}.}
\item{W_delta}{A matrix; passed to \code{as.matrix}.}
\item{delta_bias}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{b_B}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{b_C}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{b_delta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{rule}{Carried through into a list the body builds. Defaults to \code{"zoh"}.}
\item{D_skip}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the mambss_native implementation. Called by \code{morie_mambss}, \code{s6_layer}. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{estimate}, \code{state}, \code{delta}, \code{L}, \code{D}, \code{N}, \code{rule}, \code{time_invariant}, \code{method}.
}
\keyword{internal}
