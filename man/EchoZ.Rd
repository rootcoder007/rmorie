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
\item{a}{Numeric; combined arithmetically in the body.}
\item{n_0}{See Usage.}
\item{z}{See Usage.}
\item{H}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{Y}, \code{echo_factor}, \code{z}, \code{H}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
\examples{
EchoZ(a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L, z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
