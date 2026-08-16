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
\item{h_hat}{See Usage.}
\item{a}{See Usage.}
\item{n_0}{See Usage.}
\item{n}{Defaults to \code{NULL}.}
\item{n_terms}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{y_hat}, \code{n}, \code{impulses}, \code{n_impulses}, \code{echo_delay}, \code{a}, \code{method}.
}
\keyword{internal}
