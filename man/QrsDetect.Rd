% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDetect}
\alias{QrsDetect}
\title{Pan-Tompkins, Section 4.3.2, eqs (4.8)-(4.18) plus search-back.  The}
\usage{
QrsDetect(x, fs = 200) 
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{200}.}
}
\description{
filter coefficients are integers designed for fs = 200 Hz and DO NOT rescale; the timing constants (150 ms integrator, 200 ms refractory) do.
}
\value{
A list with \code{qrs}, \code{rr}, \code{hr}, \code{integrated}, \code{bandpass}, \code{delay}, \code{spki}, \code{npki}, \code{thresh1}, \code{thresh2}, \code{searchback}, \code{fs}, \code{fsnote}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(6)
fs <- 200
tv <- seq(0, 4, by = 1 / fs)
ecg <- rep(0, length(tv))
beats <- seq(0.4, 3.8, by = 0.8)
for (b in beats) {
  i <- round(b * fs)
  ecg[i:(i + 6)] <- c(0.1, 0.4, 1.2, -0.5, 0.1, 0.05, 0)
}
ecg <- ecg + rnorm(length(tv), 0, 0.02)
QrsDetect(ecg, fs = fs)
}
