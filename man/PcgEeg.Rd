% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgEeg}
\alias{PcgEeg}
\title{PcgEeg: PCG-EEG magnitude coherence; Rangayyan (2024) eq. (4.32)}
\usage{
PcgEeg(pcg, eeg, fs, n_segments = 8, band = c(1, 100)) 
}
\arguments{
\item{pcg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{eeg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{n_segments}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{band}{A vector; indexed elementwise. Defaults to \code{c(1, 100)}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{freq_hz}, \code{coherence}, \code{coherence_sq}, \code{phase_rad}, \code{peak_coherence}, \code{peak_freq_hz}, \code{mean_coherence}, \code{delay_ms_at_peak}, \code{significance_level}, \code{n_segments}, \code{segment_samples}, \code{band_hz}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(2)
fs <- 200
n <- 1024
pcg <- sin(2 * pi * 60 * (1:n) / fs) + 0.2 * rnorm(n)
eeg <- 0.4 * pcg + rnorm(n)
r <- PcgEeg(pcg, eeg, fs = fs)
str(r, max.level = 1)
}
