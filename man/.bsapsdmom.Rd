% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsapsdmom}
\alias{.bsapsdmom}
\title{Moments of a PSD treated as a density, Rangayyan (2024) Section 6.4.1}
\usage{
.bsapsdmom(freqs, power)
}
\arguments{
\item{freqs}{A vector; its length is taken and its elements indexed.}
\item{power}{Numeric; passed to \code{cumsum}.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{CadAcou}, \code{CadSpec}, \code{EggFeat} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{total_power}, \code{mean_freq_hz}, \code{median_freq_hz}, \code{fm2_hz2}, \code{spread_hz}, \code{spectral_skewness}, \code{spectral_kurtosis}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bsapsdmom(V, V)
}
