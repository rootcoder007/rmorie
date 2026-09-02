% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoSpec}
\alias{EchoSpec}
\title{Fourier spectrum of a wavelet with an echo, eq (4.77)}
\usage{
EchoSpec(a, n_0, omega, H = NULL) 
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{n_0}{Coerced to integer by the body, with \code{as.integer}.}
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{H}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{Y}, \code{echo_factor}, \code{magnitude}, \code{phase}, \code{omega}, \code{ripple_period}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
\examples{
EchoSpec(a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L, omega = c(1, 2, 3, 4, 5, 6, 7, 8))
}
