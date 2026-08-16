% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Euler}
\alias{Euler}
\title{Eq (3.74): exp(j omega t) = cos(omega t) + j sin(omega t)}
\usage{
Euler(omega, t = 0)
}
\arguments{
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the rangayyan_xfrm implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{value}, \code{real}, \code{imag}, \code{angle}, \code{unit_modulus}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Euler(V)
}
