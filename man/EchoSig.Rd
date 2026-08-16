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
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{n_0}{Coerced to integer by the body, with \code{as.integer}.}
\item{n}{Passed to \code{.tf_echo_idx}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{n}, \code{h}, \code{a}, \code{n_0}, \code{echo_visible}, \code{method}.
}
\keyword{internal}
\examples{
EchoSig(h = 0.5, a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L)
}
