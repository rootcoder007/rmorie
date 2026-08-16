% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_gev_trend}
\alias{morie_evt_gev_trend}
\title{Nonstationary GEV with a linear trend in location}
\usage{
morie_evt_gev_trend(x, t = seq_along(x) - 1)
}
\arguments{
\item{x}{series of maxima; t optional time index}
}
\description{
(Coles 2001 sec. 6.2)
}
\value{
A list with \code{beta0}, \code{beta1}, \code{sigma}, \code{xi}, \code{loglik}, \code{lr_vs_stationary}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_evt_gev_trend(V)
}
