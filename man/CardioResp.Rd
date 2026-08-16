% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CardioResp}
\alias{CardioResp}
\title{CardioResp}
\usage{
CardioResp(ecg_rate, resp, fs, band = c(0.15, 0.4), nperseg = NULL)
}
\arguments{
\item{ecg_rate}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{resp}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{band}{A vector; indexed elementwise. Defaults to \code{c(0.15, 0.4)}.}
\item{nperseg}{Passed to \code{Cohere}.}
}
\description{
A step of the rangayyan_corr implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{plv}, \code{mean_phase_difference}, \code{phase_difference}, \code{coherence_peak}, \code{coherence_mean}, \code{coherence}, \code{freqs}, \code{band}, \code{n}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
n <- 1024
fs <- 128
resp <- sin(2 * pi * 0.25 * (0:(n - 1))/fs)
ecg <- sin(2 * pi * 0.25 * (0:(n - 1))/fs + 0.7)
CardioResp(ecg, resp, fs = fs)
}
