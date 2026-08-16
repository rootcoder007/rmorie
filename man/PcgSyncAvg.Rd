% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgSyncAvg}
\alias{PcgSyncAvg}
\title{S_avg(f) = (1/M) sum |PCG_k(f)|^2.  The averaging is of POWER}
\usage{
PcgSyncAvg(cycles, fs = 1)
}
\arguments{
\item{cycles}{Iterated over elementwise, with \code{lapply}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
SPECTRA, not waveforms, and that is the point: successive PCG cycles are not sample-aligned -- a murmur is turbulent, its phase differs cycle to cycle -- so waveform averaging as in eq (3.96) would cancel exactly the murmur being measured.  Both are computed so the difference is visible.
}
\value{
A list with \code{average_psd}, \code{psd_of_average}, \code{mean_waveform}, \code{freqs}, \code{m}, \code{n}, \code{power_retained}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
PcgSyncAvg(D)
}
