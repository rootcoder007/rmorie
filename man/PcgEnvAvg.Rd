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
\item{pcg}{Passed to \code{.tf_need}.}
\item{ecg}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1000}.}
\item{cycle_len}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{envelope_smoothing}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
5.5.3 Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{average_envelope}, \code{n_cycles}, \code{cycle_len}, \code{triggers}, \code{s1_index}, \code{s1_time}, \code{s1_amplitude}, \code{s2_index}, \code{s2_time}, \code{s2_amplitude}, \code{s2_s1_ratio}, \code{snr_gain_db}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
fs <- 1000
tv <- seq(0, 3, by = 1 / fs)
n <- length(tv)
ecg <- rep(0, n)
for (b in seq(0.5, 2.5, by = 1)) {
  i <- round(b * fs)
  ecg[i:(i + 6)] <- c(0.1, 0.4, 1.2, -0.5, 0.1, 0.05, 0)
}
pcg <- sin(2 * pi * 60 * tv) * exp(-((tv \%\% 1) / 0.1)) +
  0.05 * rnorm(n)
r <- PcgEnvAvg(pcg, ecg, fs = fs)
str(r, max.level = 1)
}
