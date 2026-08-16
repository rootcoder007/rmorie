% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PdMonitor}
\alias{PdMonitor}
\title{PdMonitor: Parkinson\\'s disease monitoring via multimodal signals}
\usage{
PdMonitor(eeg, emg, gait, fs, tremor_band = c(3, 7))
}
\arguments{
\item{eeg}{See Usage.}
\item{emg}{See Usage.}
\item{gait}{See Usage.}
\item{fs}{See Usage.}
\item{tremor_band}{Defaults to \code{c(3, 7)}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{eeg_bands}, \code{eeg_beta_fraction}, \code{emg_tremor_fraction}, \code{emg_tremor_freq_hz}, \code{gait_tremor_fraction}, \code{gait_tremor_freq_hz}, \code{emg_form_factor}, \code{emg_turns_per_second}, \code{gait_rate_hz}, \code{gait_regularity}, \code{tremor_present}, \code{tremor_band_hz}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
