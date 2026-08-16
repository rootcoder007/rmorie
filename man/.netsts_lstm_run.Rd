% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.netsts_lstm_run}
\alias{.netsts_lstm_run}
\title{.netsts_lstm_run}
\usage{
.netsts_lstm_run(X, W, b, hidden, forget_bias = 0)
}
\arguments{
\item{X}{A vector; its length is taken and its elements indexed.}
\item{W}{Passed to \code{.netsts_lstm_cell}.}
\item{b}{Passed to \code{.netsts_lstm_cell}.}
\item{hidden}{Coerced to integer by the body, with \code{as.integer}.}
\item{forget_bias}{Passed to \code{.netsts_lstm_cell}. Defaults to \code{0}.}
}
\description{
A step of the netsts_native implementation. Called by \code{morie_netsts}. See the file header for the source the module follows.
}
\value{
A list with \code{hs}, \code{cs}, \code{gates}.
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
