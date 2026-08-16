% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SeizWt}
\alias{SeizWt}
\title{Fluctuation intensity of DWT coefficients, Sec 8.17 eq (8.132)}
\usage{
SeizWt(eeg, fs = 1, wavelet = "db4", levels = 5, scales = c(3, 4, 5), threshold = NULL)
}
\arguments{
\item{eeg}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{wavelet}{Defaults to \code{"db4"}.}
\item{levels}{Defaults to \code{5}.}
\item{scales}{Defaults to \code{c(3, 4, 5)}.}
\item{threshold}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{fi}, \code{fi_total}, \code{scales}, \code{bands}, \code{energies}, \code{seizure_detected}, \code{threshold}, \code{wavelet}, \code{levels}, \code{method}.
}
\keyword{internal}
