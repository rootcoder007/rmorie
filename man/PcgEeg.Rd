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
\item{pcg}{See Usage.}
\item{eeg}{See Usage.}
\item{fs}{See Usage.}
\item{n_segments}{Defaults to \code{8}.}
\item{band}{Defaults to \code{c(1, 100)}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{freq_hz}, \code{coherence}, \code{coherence_sq}, \code{phase_rad}, \code{peak_coherence}, \code{peak_freq_hz}, \code{mean_coherence}, \code{delay_ms_at_peak}, \code{significance_level}, \code{n_segments}, \code{segment_samples}, \code{band_hz}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
