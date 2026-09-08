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
\item{eeg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{eog}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{emg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{epoch_len}{Numeric; combined arithmetically in the body. Defaults to \code{30}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{epochs}, \code{stage_sequence}, \code{stage_minutes}, \code{total_sleep_time_min}, \code{recording_time_min}, \code{sleep_efficiency}, \code{n_epochs}, \code{epoch_len_s}, \code{fs_hz}, \code{heuristic}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
fs <- 100
n <- fs * 90
eeg <- rnorm(n)
eog <- rnorm(n, 0, 0.5)
emg <- rnorm(n, 0, 0.8)
r <- PsgStage(eeg, eog, emg, fs = fs, epoch_len = 30)
str(r, max.level = 1)
}
