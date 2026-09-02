% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDeriv}
\alias{QrsDeriv}
\title{Balda et al., Section 4.3.1: y0 by eq (4.1), y1 by eq (4.2), combined}
\usage{
QrsDeriv(x, fs, thresh = 1) 
}
\arguments{
\item{x}{Numeric; passed to \code{abs}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{thresh}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
as y2 = 1.3 y0 + 1.1 y1 (eq 4.3), scanned with a threshold of 1.0 on a maximum-normalised ECG.  Six of eight successive samples must pass, which is what makes the decision robust to single-sample derivative spikes.
}
\value{
A list with \code{qrs}, \code{y0}, \code{y1}, \code{y2}, \code{y3}, \code{mask}, \code{thresh}, \code{fs}, \code{hr}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(6)
fs <- 200
tv <- seq(0, 4, by = 1 / fs)
ecg <- rep(0, length(tv))
for (b in seq(0.4, 3.8, by = 0.8)) {
  i <- round(b * fs)
  ecg[i:(i + 6)] <- c(0.1, 0.4, 1.2, -0.5, 0.1, 0.05, 0)
}
ecg <- ecg + rnorm(length(tv), 0, 0.02)
QrsDeriv(ecg, fs = fs)
}
