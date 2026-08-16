% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EcgFeat}
\alias{EcgFeat}
\title{Section 1.2.4 waves and intervals.  Amplitudes are measured against}
\usage{
EcgFeat(x, qrs, fs)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
the PQ segment, not against zero: the PQ segment is the isoelectric reference the book names, and it absorbs any residual baseline offset.
}
\value{
A list with \code{pamp}, \code{qamp}, \code{ramp}, \code{samp}, \code{tamp}, \code{qrsdur}, \code{pdur}, \code{tdur}, \code{prdur}, \code{qtdur}, \code{qrsdurmean}, \code{prdurmean}, \code{qtdurmean}, \code{rampmean}, \code{nbeats}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(9)
fs <- 200
tv <- seq(0, 4, by = 1 / fs)
ecg <- rep(0, length(tv))
qrs <- round(seq(0.4, 3.8, by = 0.8) * fs)
for (i in qrs) ecg[i:(i + 6)] <- c(0.1, 0.4, 1.2, -0.5, 0.1, 0.05, 0)
ecg <- ecg + rnorm(length(tv), 0, 0.02)
EcgFeat(ecg, qrs = qrs, fs = fs)
}
