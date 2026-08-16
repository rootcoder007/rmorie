% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nhits_expressiveness_knots}
\alias{.nhits_expressiveness_knots}
\title{.nhits_expressiveness_knots}
\usage{
.nhits_expressiveness_knots(horizon, ratio)
}
\arguments{
\item{horizon}{Coerced to integer by the body, with \code{as.integer}.}
\item{ratio}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the nhits_native implementation. Called by \code{.nhits_nhits_block}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\references{
----------
Challu, C., Olivares, K. G., Oreshkin, B. N., Garza, F.,
Mergenthaler-Canseco, M. & Dubrawski, A. (2023) "NHITS: Neural
Hierarchical Interpolation for Time Series Forecasting", *Proceedings
of the AAAI Conference on Artificial Intelligence* 37(6), 6989-6997,
doi:10.1609/aaai.v37i6.25854, arXiv:2201.12886. Sec. 3: multi-rate
sampling, hierarchical interpolation, and the expressiveness ratios.

Oreshkin, B. N., Carpov, D., Chapados, N. & Bengio, Y. (2020)
"N-BEATS: Neural basis expansion analysis for interpretable time series
forecasting", *International Conference on Learning Representations*,
arXiv:1905.10437. The doubly residual stack N-HiTS inherits.
}
\keyword{internal}
