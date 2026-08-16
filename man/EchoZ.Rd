% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoZ}
\alias{EchoZ}
\title{Z-transform of a wavelet with an echo, eq (4.76)}
\usage{
EchoZ(a, n_0, z, H = NULL)
}
\arguments{
\item{a}{See Usage.}
\item{n_0}{See Usage.}
\item{z}{See Usage.}
\item{H}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{Y}, \code{echo_factor}, \code{z}, \code{H}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
