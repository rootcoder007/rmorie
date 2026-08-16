% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EmgFreq}
\alias{EmgFreq}
\title{Eqs (6.34)-(6.35) on the EMG periodogram.  Both fall as a muscle}
\usage{
EmgFreq(x, fs, nperseg = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nperseg}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
fatigues, but they are not interchangeable: the median is far less sensitive to the high-frequency tail, where an EMG record carries mostly instrumentation noise, so it is the more stable fatigue index and the two diverge exactly when the tail is contaminated.
}
\value{
A list with \code{mean_frequency}, \code{median_frequency}, \code{difference}, \code{bandwidth}, \code{total_power}, \code{psd}, \code{freqs}, \code{fs}, \code{nperseg}, \code{method}.
}
\keyword{internal}
\examples{
EmgFreq(x = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
