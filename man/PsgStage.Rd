% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PsgStage}
\alias{PsgStage}
\title{PsgStage: polysomnography signal fusion for sleep staging}
\usage{
PsgStage(eeg, eog, emg, fs, epoch_len = 30)
}
\arguments{
\item{eeg}{See Usage.}
\item{eog}{See Usage.}
\item{emg}{See Usage.}
\item{fs}{See Usage.}
\item{epoch_len}{Defaults to \code{30}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{epochs}, \code{stage_sequence}, \code{stage_minutes}, \code{total_sleep_time_min}, \code{recording_time_min}, \code{sleep_efficiency}, \code{n_epochs}, \code{epoch_len_s}, \code{fs_hz}, \code{heuristic}, \code{units}, \code{method}.
}
\keyword{internal}
