% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CDemod}
\alias{CDemod}
\title{Complex demodulation, Sec 5.5.1 eqs (5.16)-(5.19)}
\usage{
CDemod(x, fs = 1, f0 = NULL, bandwidth = NULL)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{f0}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{bandwidth}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{amplitude}, \code{phase}, \code{demodulated}, \code{f0}, \code{bandwidth}, \code{mean_amplitude}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CDemod(V)
}
