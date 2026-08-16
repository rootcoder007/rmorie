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
\item{x}{Passed to \code{.tf_need}.}
\item{wavelet}{Passed to \code{.tf_dwt}. Defaults to \code{"db4"}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{WtEntropy}. See the file header for the source the module follows.
}
\value{
A list with \code{energies}, \code{relative}, \code{labels}, \code{total_energy}, \code{input_energy}, \code{energy_balance}, \code{dominant_band}, \code{levels}, \code{wavelet}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(6)
x <- sin(2 * pi * (1:128) / 16) + rnorm(128, 0, 0.1)
WtEnergy(x, wavelet = "db4", levels = 3)
}
