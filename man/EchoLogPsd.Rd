% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoLogPsd}
\alias{EchoLogPsd}
\title{Log power spectrum of a wavelet with an echo, eq (4.85)}
\usage{
EchoLogPsd(H, a, n_0, omega)
}
\arguments{
\item{H}{A vector; its length is taken.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{n_0}{See Usage.}
\item{omega}{See Usage.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{log_power}, \code{wavelet_log_power}, \code{echo_log_power}, \code{dc_term}, \code{ripple}, \code{modulation_index}, \code{ripple_period}, \code{decomposition_error}, \code{omega}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
\examples{
EchoLogPsd(H = 0.5, a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L, omega = c(1, 2, 3, 4, 5, 6, 7, 8))
}
