% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgParts}
\alias{PcgParts}
\title{The six-step procedure of Section 4.9.  Timing is imported from the}
\usage{
PcgParts(pcg, ecg, cp, fs) 
}
\arguments{
\item{pcg}{A vector; its length is taken and its elements indexed.}
\item{ecg}{A vector; its length is taken.}
\item{cp}{A vector; its length is taken.}
\item{fs}{Passed to \code{.morie_qrs_fs}.}
}
\description{
ECG and the carotid pulse because S1 and S2 are not reliably the loudest events in a PCG once murmurs are present.
}
\value{
A list with \code{s1}, \code{s2}, \code{systole}, \code{diastole}, \code{systolerms}, \code{diastolerms}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
fs <- 250
tv <- seq(0, 3, by = 1 / fs)
n <- length(tv)
ecg <- rep(0, n)
for (b in seq(0.4, 2.8, by = 0.8)) {
  i <- round(b * fs); ecg[i:(i + 5)] <- c(0.2, 1.2, -0.5, 0.1, 0.05, 0)
}
ecg <- ecg + 0.01 * rnorm(n)
ph <- ((tv - 0.45) \%\% 0.8) / 0.8
cp <- exp(-((ph - 0.25) / 0.12)^2) + 0.35 * exp(-((ph - 0.55) / 0.05)^2)
pcg <- sin(2 * pi * 60 * tv) * exp(-((tv \%\% 0.8) / 0.06)) +
  0.02 * rnorm(n)
r <- PcgParts(pcg, ecg, cp, fs = fs)
str(r, max.level = 1)
}
