% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoCep}
\alias{EchoCep}
\title{Complex cepstrum of a wavelet with an echo, eq (4.80)}
\usage{
EchoCep(h_hat, a, n_0, n = NULL, n_terms = NULL) 
}
\arguments{
\item{h_hat}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{a}{Numeric; passed to \code{abs}.}
\item{n_0}{Coerced to integer by the body, with \code{as.integer}.}
\item{n}{Optional; may be \code{NULL}. Passed to \code{.tf_echo_idx}.}
\item{n_terms}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{y_hat}, \code{n}, \code{impulses}, \code{n_impulses}, \code{echo_delay}, \code{a}, \code{method}.
}
\keyword{internal}
