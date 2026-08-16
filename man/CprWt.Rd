% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CprWt}
\alias{CprWt}
\title{Wavelet scale distribution width of a fibrillation waveform, Sec 8.15}
\usage{
CprWt(ecg, fs = 250, scales = NULL, w0 = 5, band = c(3, 21))
}
\arguments{
\item{ecg}{See Usage.}
\item{fs}{Defaults to \code{250}.}
\item{scales}{Defaults to \code{NULL}.}
\item{w0}{Defaults to \code{5}.}
\item{band}{Defaults to \code{c(3, 21)}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{sdw}, \code{scale_energy}, \code{scales}, \code{freqs}, \code{peak_scale}, \code{peak_freq}, \code{organised}, \code{band}, \code{method}.
}
\keyword{internal}
