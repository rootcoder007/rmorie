% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_correlogram}
\alias{.sp_correlogram}
\title{Internal: isotropic correlogram R(h) for the Schabenberger models.}
\usage{
.sp_correlogram(h, rng, model)
}
\arguments{
\item{h}{Numeric vector of non-negative lags.}
\item{rng}{Practical (or true) range, positive.}
\item{model}{One of "exponential", "gaussian", "spherical", "wave".}
}
\description{
The book parameterises the exponential and gaussian models by the PRACTICAL range alpha: the lag at which correlation has fallen to exp(-3) = 0.049787 ("0.05 or less", p. 143). The spherical model is different -- it has a TRUE range and is exactly zero at h = alpha.
}
\value{
Numeric vector R(h).
}
\keyword{internal}
