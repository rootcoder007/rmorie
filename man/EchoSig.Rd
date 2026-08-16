% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoSig}
\alias{EchoSig}
\title{Wavelet plus echo in the time domain, eq (4.75)}
\usage{
EchoSig(h, a, n_0, n = NULL)
}
\arguments{
\item{h}{See Usage.}
\item{a}{See Usage.}
\item{n_0}{See Usage.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{y}, \code{n}, \code{h}, \code{a}, \code{n_0}, \code{echo_visible}, \code{method}.
}
\keyword{internal}
\examples{
EchoSig(h = 0.5, a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L)
}
