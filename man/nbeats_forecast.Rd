% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nbeats_forecast}
\alias{nbeats_forecast}
\title{nbeats_forecast}
\usage{
nbeats_forecast(y, horizon, lookback = NULL, blocks = NULL, ridge = 1e-08)
}
\arguments{
\item{y}{See Usage.}
\item{horizon}{See Usage.}
\item{lookback}{Defaults to \code{NULL}.}
\item{blocks}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
A step of the nbeats_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{forecast}, \code{residual}, \code{backcast}, \code{blocks}, \code{lookback}, \code{horizon}, \code{n}, \code{residual_norm}, \code{window_norm}, \code{n_blocks}, \code{method}.
}
\keyword{internal}
\examples{
nbeats_forecast(y = c(1, 2, 3, 4, 5, 6, 7, 8), horizon = 5L)
}
