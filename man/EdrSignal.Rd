% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EdrSignal}
\alias{EdrSignal}
\title{Section 2.2.4 for the physiology; the estimator is Arunachalam and}
\usage{
EdrSignal(x, qrs, fs, fsr = 4)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{fsr}{Numeric; combined arithmetically in the body. Defaults to \code{4}.}
}
\description{
Brown, Proc. IEEE EMBC 2009, pp. 5681-5684 (reference [52] of Chapter 2). Chest motion swings the cardiac electrical axis, so the R amplitude sampled once per beat is a respiratory signal without a respiration sensor.
}
\value{
A list with \code{edr}, \code{amp}, \code{times}, \code{resprate}, \code{fsr}, \code{nbeats}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(9)
fs <- 200
tv <- seq(0, 10, by = 1 / fs)
ecg <- rep(0, length(tv))
qrs <- round(seq(0.4, 9.8, by = 0.8) * fs)
amp <- 1 + 0.2 * sin(2 * pi * 0.25 * (seq_along(qrs)) * 0.8)
for (k in seq_along(qrs)) {
  i <- qrs[k]
  ecg[i:(i + 6)] <- amp[k] * c(0.1, 0.4, 1.2, -0.5, 0.1, 0.05, 0)
}
EdrSignal(ecg, qrs = qrs, fs = fs)
}
