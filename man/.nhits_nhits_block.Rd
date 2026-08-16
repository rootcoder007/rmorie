% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nhits_nhits_block}
\alias{.nhits_nhits_block}
\title{.nhits_nhits_block}
\usage{
.nhits_nhits_block(window, horizon, kernel = 1L, ratio = 1, degree = 2L, ridge = 1e-08)
}
\arguments{
\item{window}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{horizon}{Coerced to integer by the body, with \code{as.integer}.}
\item{kernel}{Passed to \code{.nhits_max_pool}. Defaults to \code{1L}.}
\item{ratio}{Passed to \code{.nhits_expressiveness_knots}. Defaults to \code{1}.}
\item{degree}{Numeric; combined arithmetically in the body. Defaults to \code{2L}.}
\item{ridge}{Passed to \code{.nhits_lstsq}. Defaults to \code{1e-08}.}
}
\description{
A step of the nhits_native implementation. Called by \code{.nhits_nhits_stack}. See the file header for the source the module follows.
}
\value{
A list with \code{backcast}, \code{forecast}, \code{knots}, \code{pooled}.
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
