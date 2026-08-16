% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoLogSp}
\alias{EchoLogSp}
\title{Complex log spectrum of a wavelet with an echo, eqs (4.78)-(4.79)}
\usage{
EchoLogSp(a, n_0, omega, H_hat = NULL, n_terms = NULL)
}
\arguments{
\item{a}{See Usage.}
\item{n_0}{See Usage.}
\item{omega}{See Usage.}
\item{H_hat}{Defaults to \code{NULL}.}
\item{n_terms}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{Y_hat}, \code{echo_log}, \code{series}, \code{series_valid}, \code{series_error}, \code{omega}, \code{a}, \code{n_0}, \code{n_terms}, \code{method}.
}
\keyword{internal}
