% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ErdErs}
\alias{ErdErs}
\title{ErdErs: event-related desynchronisation / synchronisation for BCI}
\usage{
ErdErs(eeg, fs, ref_window, active_window, band = c(8, 13)) 
}
\arguments{
\item{eeg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{ref_window}{A vector; indexed elementwise.}
\item{active_window}{A vector; indexed elementwise.}
\item{band}{A vector; indexed elementwise. Defaults to \code{c(8, 13)}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{erd_percent}, \code{ref_power}, \code{active_power}, \code{power_ratio}, \code{event}, \code{band_hz}, \code{ref_window_s}, \code{active_window_s}, \code{ref_samples}, \code{active_samples}, \code{fs_hz}, \code{sign_convention}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(8)
fs <- 100
eeg <- rnorm(fs * 6)
ErdErs(eeg, fs = fs, ref_window = c(0, 2), active_window = c(3, 5))
}
