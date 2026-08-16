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
\item{eeg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{epoch_s}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{ratio_threshold}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{baseline_epochs}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{epochs}, \code{seizure_detected}, \code{n_flagged}, \code{seizure_intervals_s}, \code{baseline_slow_fraction}, \code{threshold_slow_fraction}, \code{baseline_epochs}, \code{n_epochs}, \code{epoch_s}, \code{fs_hz}, \code{binnie_bands_hz}, \code{units}, \code{method}.
}
\keyword{internal}
