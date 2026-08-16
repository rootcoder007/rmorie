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
\item{H}{A vector; its length is taken.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{n_0}{Coerced to integer by the body, with \code{as.integer}.}
\item{z}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{power}, \code{wavelet_power}, \code{echo_power}, \code{z}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
\examples{
EchoPsd(H = 0.5, a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L, z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
