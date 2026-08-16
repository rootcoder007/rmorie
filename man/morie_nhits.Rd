% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_nhits}
\alias{morie_nhits}
\title{morie_nhits}
\usage{
morie_nhits(y, horizon, lookback = NULL, blocks = NULL, ridge = 1e-08)
}
\arguments{
\item{y}{See Usage.}
\item{horizon}{See Usage.}
\item{lookback}{Defaults to \code{NULL}.}
\item{blocks}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
Part of the nhits_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{forecast}, \code{residual}, \code{blocks}, \code{lookback}, \code{horizon}, \code{n}, \code{total_knots}, \code{dense_parameters}, \code{residual_norm}, \code{n_blocks}, \code{method}.
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
