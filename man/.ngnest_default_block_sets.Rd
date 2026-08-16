% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ngnest_default_block_sets}
\alias{.ngnest_default_block_sets}
\title{.ngnest_default_block_sets}
\usage{
.ngnest_default_block_sets()
}
\description{
A step of the ngnest_native implementation. Called by \code{.ngnest_ensemble_members}. See the file header for the source the module follows.
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
