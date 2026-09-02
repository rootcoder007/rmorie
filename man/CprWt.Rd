% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CprWt}
\alias{CprWt}
\title{Wavelet scale distribution width of a fibrillation waveform, Sec 8.15}
\usage{
CprWt(ecg, fs = 250, scales = NULL, w0 = 5, band = c(3, 21)) 
}
\arguments{
\item{ecg}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{250}.}
\item{scales}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{w0}{Numeric; combined arithmetically in the body. Defaults to \code{5}.}
\item{band}{A vector; indexed elementwise. Defaults to \code{c(3, 21)}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{sdw}, \code{scale_energy}, \code{scales}, \code{freqs}, \code{peak_scale}, \code{peak_freq}, \code{organised}, \code{band}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
fs <- 250
tvec <- seq(0, 2, by = 1 / fs)
ecg <- sin(2 * pi * 8 * tvec) + 0.2 * rnorm(length(tvec))
CprWt(ecg, fs = fs)
}
