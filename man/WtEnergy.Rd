% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WtEnergy}
\alias{WtEnergy}
\title{Wavelet subband energy, Sec 8.15}
\usage{
WtEnergy(x, wavelet = "db4", levels = 3)
}
\arguments{
\item{x}{See Usage.}
\item{wavelet}{Defaults to \code{"db4"}.}
\item{levels}{Defaults to \code{3}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{energies}, \code{relative}, \code{labels}, \code{total_energy}, \code{input_energy}, \code{energy_balance}, \code{dominant_band}, \code{levels}, \code{wavelet}, \code{method}.
}
\keyword{internal}
