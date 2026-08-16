% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.netsts_lstsq}
\alias{.netsts_lstsq}
\title{.netsts_lstsq}
\usage{
.netsts_lstsq(X, y, ridge)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the netsts_native implementation. Called by \code{morie_netsts}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
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
\examples{
.netsts_lstsq(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), ridge = 5L)
}
