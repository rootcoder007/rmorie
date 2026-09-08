% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsMonitor}
\alias{RlsMonitor}
\title{RlsMonitor}
\usage{
RlsMonitor(x, reference = NULL, order = 8, lam = 0.98, settle = NULL, threshold = 3, 
    window = NULL) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{reference}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{lam}{Passed to \code{RlsFilt}. Defaults to \code{0.98}.}
\item{settle}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.}
\item{window}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{error}, \code{error_power}, \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{baseline}, \code{baseline_sd}, \code{settle}, \code{window}, \code{order}, \code{transient_excluded}, \code{method}.
}
\keyword{internal}
\examples{
sine_a <- function(n, cycles, amp = 1, phase = 0) amp * sin(2 * 
    pi * cycles * (0:(n - 1))/n + phase)
n <- 400
v <- sine_a(256, 3)
ref <- sine_a(n, 61)
Anc(v + 0.8 * ref, ref, order = 4, mu = 0.005)
RlsMonitor(x = ref)
}
