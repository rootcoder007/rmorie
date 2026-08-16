% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgEnvAvg}
\alias{PcgEnvAvg}
\title{ECG-triggered synchronised averaging of PCG envelopes, Sec 3.5 +}
\usage{
PcgEnvAvg(pcg, ecg, fs = 1000, cycle_len = NULL, envelope_smoothing = NULL)
}
\arguments{
\item{pcg}{See Usage.}
\item{ecg}{See Usage.}
\item{fs}{Defaults to \code{1000}.}
\item{cycle_len}{Defaults to \code{NULL}.}
\item{envelope_smoothing}{Defaults to \code{NULL}.}
}
\description{
5.5.3 Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{average_envelope}, \code{n_cycles}, \code{cycle_len}, \code{triggers}, \code{s1_index}, \code{s1_time}, \code{s1_amplitude}, \code{s2_index}, \code{s2_time}, \code{s2_amplitude}, \code{s2_s1_ratio}, \code{snr_gain_db}, \code{method}.
}
\keyword{internal}
