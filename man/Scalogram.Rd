% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Scalogram}
\alias{Scalogram}
\title{Scalogram (|CWT|^2), eq (8.107) and Figure 8.29}
\usage{
Scalogram(x, fs = 1, scales = NULL, wavelet = "morlet", w0 = 5)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{scales}{Defaults to \code{NULL}.}
\item{wavelet}{Defaults to \code{"morlet"}.}
\item{w0}{Defaults to \code{5}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{scalogram}, \code{scales}, \code{freqs}, \code{times}, \code{energy_per_scale}, \code{total_energy}, \code{ridge}, \code{method}.
}
\keyword{internal}
