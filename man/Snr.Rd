% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Snr}
\alias{Snr}
\title{Section 3.2.1 gives two definitions in one sentence, and they are not}
\usage{
Snr(signal, noise, definition = "power")
}
\arguments{
\item{signal}{See Usage.}
\item{noise}{See Usage.}
\item{definition}{One of \code{"peak"}, \code{"power"}. Defaults to \code{"power"}.}
}
\description{
interchangeable: the power ratio (10 log10) and the peak-to-peak amplitude over the noise RMS (20 log10, roughly 9 dB higher for a sinusoid).  Both are returned; snr_db is the one named.
}
\value{
A list with \code{snr_db}, \code{snr_power_db}, \code{snr_peak_db}, \code{signal_power}, \code{noise_power}, \code{noise_rms}, \code{definition}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Snr(V, V)
}
