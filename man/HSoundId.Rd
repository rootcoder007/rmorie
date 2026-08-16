% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HSoundId}
\alias{HSoundId}
\title{Section 4.9.  S1 at the QRS onset; S2 from the dicrotic notch minus}
\usage{
HSoundId(ecg, cp, fs)
}
\arguments{
\item{ecg}{A vector; its length is taken.}
\item{cp}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
the standardised 52.6 ms (mean + 2 SD of the 42.6 +/- 5 ms Lehner and Rangayyan measurement).  The notch lags the sound because it is the same aortic-valve closure observed after propagation up the arterial tree.
}
\value{
A list with \code{s1}, \code{s2}, \code{notch}, \code{qrs}, \code{s2delayms}, \code{s2delaymeasured}, \code{searchwindowms}, \code{fs}, \code{method}.
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
r <- HSoundId(ecg, cp, fs = fs)
str(r, max.level = 1)
}
