% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SeizDet}
\alias{SeizDet}
\title{SeizDet: epileptic seizure detection in EEG}
\usage{
SeizDet(eeg, fs, epoch_s = 1, ratio_threshold = 2, baseline_epochs = NULL)
}
\arguments{
\item{eeg}{See Usage.}
\item{fs}{See Usage.}
\item{epoch_s}{Defaults to \code{1}.}
\item{ratio_threshold}{Defaults to \code{2}.}
\item{baseline_epochs}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{epochs}, \code{seizure_detected}, \code{n_flagged}, \code{seizure_intervals_s}, \code{baseline_slow_fraction}, \code{threshold_slow_fraction}, \code{baseline_epochs}, \code{n_epochs}, \code{epoch_s}, \code{fs_hz}, \code{binnie_bands_hz}, \code{units}, \code{method}.
}
\keyword{internal}
