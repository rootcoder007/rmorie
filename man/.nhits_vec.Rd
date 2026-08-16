% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nhits_vec}
\alias{.nhits_vec}
\title{.nhits_vec}
\usage{
.nhits_vec(y)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the nhits_native implementation. Called by \code{morie_nhits}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.nhits_vec(V)
}
