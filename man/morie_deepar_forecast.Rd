% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_deepar_forecast}
\alias{morie_deepar_forecast}
\title{Fit, sample, and read the quantiles off the trajectories}
\usage{
morie_deepar_forecast(z, horizon, n_lags = 2L, likelihood = "negative-binomial", n_samples = 300L, 
    quantiles = c(0.1, 0.5, 0.9), seed = 0L) 
}
\arguments{
\item{z}{Numeric vector of history observations.}
\item{horizon}{Integer forecast horizon.}
\item{n_lags}{Integer number of lags.}
\item{likelihood}{One of \code{"negative-binomial"} or   \code{"gaussian"}.}
\item{n_samples}{Integer number of trajectories.}
\item{quantiles}{Numeric vector of quantiles in (0, 1).}
\item{seed}{Integer seed.}
}
\description{
Fit, sample, and read the quantiles off the trajectories
}
\value{
A list with \code{mean}, \code{quantiles}, \code{paths},   \code{width} and the same metadata as the Python arm.
}
\keyword{internal}
\examples{
morie_deepar_forecast(z = c(1, 2, 3, 4, 5, 6, 7, 8), horizon = 5L)
}
