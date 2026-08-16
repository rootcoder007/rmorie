% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_welch_csd}
\alias{.morie_dsp_welch_csd}
\title{Internal helper: Welch cross-spectral density}
\usage{
.morie_dsp_welch_csd(x, y = x, nfft = 256L, fs = 1, overlap = 0.5)
}
\description{
Segments of length `nfft` with 50 percent overlap, Hamming window, averaged cross-periodograms. `y = x` gives the PSD.
}
\value{
A list with \code{freq}, \code{spec}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_dsp_welch_csd(V)
}
