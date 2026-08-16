% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwHpDft}
\alias{BwHpDft}
\title{Eq (3.149): the lowpass with the ratio inverted.  At k = 0 the ratio}
\usage{
BwHpDft(K, kc = NULL, N = 2, fc = NULL, fs = NULL)
}
\arguments{
\item{K}{See Usage.}
\item{kc}{Defaults to \code{NULL}.}
\item{N}{Defaults to \code{2}.}
\item{fc}{Defaults to \code{NULL}.}
\item{fs}{Defaults to \code{NULL}.}
}
\description{
is unbounded and the response is exactly nought -- the filter the book uses to strip baseline drift, eighth order at 2 Hz.  It leaves high-frequency noise untouched; a highpass is not a denoiser.
}
\value{
A list with \code{squared_magnitude}, \code{magnitude}, \code{half_spectrum}, \code{K}, \code{kc}, \code{N}, \code{dc_gain}, \code{reflected}, \code{leaves_high_frequency_noise_untouched}, \code{method}.
}
\keyword{internal}
