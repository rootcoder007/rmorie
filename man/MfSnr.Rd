% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfSnr}
\alias{MfSnr}
\title{Eq (4.39): a PEAK-to-MEAN ratio, not the mean-to-mean of an ordinary}
\usage{
MfSnr(my, noise_power) 
}
\arguments{
\item{my}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{noise_power}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
SNR.  The matched filter maximizes the output at ONE INSTANT, which is the right criterion for a known transient and the wrong one for a continuous signal.
}
\value{
A list with \code{snr}, \code{snr_db}, \code{amplitude_snr}, \code{my}, \code{noise_power}, \code{peak_to_mean}, \code{method}.
}
\keyword{internal}
\examples{
MfSnr(my = 5L, noise_power = 5L)
}
