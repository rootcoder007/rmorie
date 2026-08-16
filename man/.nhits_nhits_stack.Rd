% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nhits_nhits_stack}
\alias{.nhits_nhits_stack}
\title{.nhits_nhits_stack}
\usage{
.nhits_nhits_stack(window, horizon, blocks, ridge = 1e-08)
}
\arguments{
\item{window}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{horizon}{Passed to \code{.nhits_nhits_block}.}
\item{blocks}{A vector; its length is taken and its elements indexed.}
\item{ridge}{Passed to \code{.nhits_nhits_block}. Defaults to \code{1e-08}.}
}
\description{
A step of the nhits_native implementation. Called by \code{morie_nhits}. See the file header for the source the module follows.
}
\value{
A list with \code{total}, \code{resid}, \code{trace}.
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
