% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_netsts}
\alias{morie_netsts}
\title{morie_netsts}
\usage{
morie_netsts(y, horizon, hidden = 8, n_lags = 4, strategy = "recursive", forget_bias = 1, seed = 0, ridge = 1e-06)
}
\arguments{
\item{y}{Passed to \code{.netsts_vec}.}
\item{horizon}{See Usage.}
\item{hidden}{Defaults to \code{8}.}
\item{n_lags}{Defaults to \code{4}.}
\item{strategy}{One of \code{"direct"}, \code{"recursive"}. Defaults to \code{"recursive"}.}
\item{forget_bias}{Passed to \code{.netsts_lstm_run}. Defaults to \code{1}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{ridge}{Passed to \code{.netsts_lstsq}. Defaults to \code{1e-06}.}
}
\description{
A step of the netsts_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{forecast}, \code{strategy}, \code{hidden}, \code{n_lags}, \code{forget_bias}, \code{mean}, \code{sd}, \code{n_models}, \code{retention_10}, \code{method}.
}
\references{
----------
Hochreiter, S. & Schmidhuber, J. (1997) "Long Short-Term Memory",
Neural Computation 9(8), 1735-1780, doi:10.1162/neco.1997.9.8.1735.
The cell, and the constant error carousel argument.

Gers, F. A., Schmidhuber, J. & Cummins, F. (2000) "Learning to Forget:
Continual Prediction with LSTM", Neural Computation 12(10),
2451-2471, doi:10.1162/089976600300015015. The forget gate itself.

Jozefowicz, R., Zaremba, W. & Sutskever, I. (2015) "An Empirical
Exploration of Recurrent Network Architectures", Proceedings of the
32nd International Conference on Machine Learning, PMLR 37,
2342-2350. The positive forget-gate bias initialisation.

Hewamalage, H., Bergmeir, C. & Bandara, K. (2021) "Recurrent Neural
Networks for Time Series Forecasting: Current status and future
directions", International Journal of Forecasting 37(1), 388-427,
doi:10.1016/j.ijforecast.2020.06.008. Recursive versus direct
strategies, and preprocessing.
}
\keyword{internal}
