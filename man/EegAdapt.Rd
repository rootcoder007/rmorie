% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EegAdapt}
\alias{EegAdapt}
\title{EegAdapt}
\usage{
EegAdapt(x, fs, window = NULL, step = NULL, order = 4, threshold = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{window}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{step}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.}
\item{threshold}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_ccf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{d}, \code{d_fixed_reference}, \code{times}, \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{median}, \code{mad}, \code{window}, \code{step}, \code{order}, \code{fs}, \code{reference_restarts_at_boundaries}, \code{robust_threshold}, \code{method}.
}
\keyword{internal}
\examples{
sine <- function(n, cycles_hz, fs = 100, amp = 1, phase = 0) {
    amp * sin(2 * pi * cycles_hz * (seq_len(n) - 1L)/fs + phase)
}
EegAdapt(c(sine(200, 5), sine(200, 40)), 100, window = 60, step = 20)
}
