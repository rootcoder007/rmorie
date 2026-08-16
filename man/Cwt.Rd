% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cwt}
\alias{Cwt}
\title{Continuous wavelet transform, eq (8.107)}
\usage{
Cwt(x, fs = 1, wavelet = "morlet", scales = NULL, w0 = 5)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{wavelet}{Passed to \code{.tf_cwt}. Defaults to \code{"morlet"}.}
\item{scales}{Defaults to \code{NULL}.}
\item{w0}{Defaults to \code{5}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{Scalogram}. See the file header for the source the module follows.
}
\value{
A list with \code{coeffs}, \code{scales}, \code{freqs}, \code{times}, \code{energy_per_scale}, \code{peak_scale}, \code{wavelet}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Cwt(V)
}
