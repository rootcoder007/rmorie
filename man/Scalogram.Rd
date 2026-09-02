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
\item{x}{Passed to \code{Cwt}.}
\item{fs}{Passed to \code{Cwt}. Defaults to \code{1}.}
\item{scales}{Passed to \code{Cwt}.}
\item{wavelet}{Passed to \code{Cwt}. Defaults to \code{"morlet"}.}
\item{w0}{Passed to \code{Cwt}. Defaults to \code{5}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{CwtRidge}. See the file header for the source the module follows.
}
\value{
A list with \code{scalogram}, \code{scales}, \code{freqs}, \code{times}, \code{energy_per_scale}, \code{total_energy}, \code{ridge}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Scalogram(V)
}
