% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ngnest_nbeats_stack}
\alias{.ngnest_nbeats_stack}
\title{Delegate to the shared implementation, which is what the Python arm}
\usage{
.ngnest_nbeats_stack(window, H, blocks, ridge = 1e-08)
}
\arguments{
\item{window}{See Usage.}
\item{H}{See Usage.}
\item{blocks}{See Usage.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
does (morie.fn.ngnest imports nbeats_stack from morie.fn.nbeats). The local copy that used to live here normalised trend time by (lb - 1) rather than lb and built the Fourier basis on max(H, lb) with 1-based time, so its forecasts drifted from the spec.
}
\value{
The value of \code{list}.
}
\references{
----------
Oreshkin, B. N., Carpov, D., Chapados, N. & Bengio, Y. (2020)
"N-BEATS: Neural basis expansion analysis for interpretable time series
forecasting", *International Conference on Learning Representations*,
arXiv:1905.10437. Sec. 3.3 (ensembling), Sec. 5.

Makridakis, S., Spiliotis, E. & Assimakopoulos, V. (2020) "The M4
Competition: 100,000 time series and 61 forecasting methods",
*International Journal of Forecasting* 36(1), 54-74,
doi:10.1016/j.ijforecast.2019.04.014. The benchmark those numbers come
from, and where combination dominated.

Bates, J. M. & Granger, C. W. J. (1969) "The Combination of Forecasts",
*Journal of the Operational Research Society* 20(4), 451-468,
doi:10.1057/jors.1969.103. Why combining decorrelated forecasts beats
choosing among them.
}
\keyword{internal}
