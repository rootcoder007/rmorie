% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoPsd}
\alias{EchoPsd}
\title{Power spectrum of a wavelet with an echo, eq (4.84)}
\usage{
EchoPsd(H, a, n_0, z)
}
\arguments{
\item{H}{See Usage.}
\item{a}{See Usage.}
\item{n_0}{See Usage.}
\item{z}{See Usage.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{power}, \code{wavelet_power}, \code{echo_power}, \code{z}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
\examples{
EchoPsd(H = 0.5, a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L, z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
