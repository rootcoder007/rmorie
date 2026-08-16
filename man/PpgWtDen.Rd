% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PpgWtDen}
\alias{PpgWtDen}
\title{Wavelet-shrinkage denoising of PPG, Sec 8.14 with eqs (8.103)-(8.105)}
\usage{
PpgWtDen(ppg, fs = 100, wavelet = "db4", levels = 4, threshold_type = "soft")
}
\arguments{
\item{ppg}{Passed to \code{.tf_need}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{100}.}
\item{wavelet}{Passed to \code{.tf_dwt}. Defaults to \code{"db4"}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.}
\item{threshold_type}{Passed to \code{WtThresh}. Defaults to \code{"soft"}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{denoised}, \code{artifact}, \code{threshold}, \code{sigma}, \code{artifact_energy}, \code{snr_improvement_db}, \code{approx_energy}, \code{levels}, \code{wavelet}, \code{fs}, \code{method}.
}
\keyword{internal}
