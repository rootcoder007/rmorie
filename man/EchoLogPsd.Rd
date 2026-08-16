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
\item{H}{See Usage.}
\item{a}{See Usage.}
\item{n_0}{See Usage.}
\item{omega}{See Usage.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{log_power}, \code{wavelet_log_power}, \code{echo_log_power}, \code{dc_term}, \code{ripple}, \code{modulation_index}, \code{ripple_period}, \code{decomposition_error}, \code{omega}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
