% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_vgm_wls_fit}
\alias{.morie_vgm_wls_fit}
\title{Fast WLS fit of a variogram model on the binned empirical variogram}
\usage{
.morie_vgm_wls_fit(coords, values, model = "exponential")
}
\arguments{
\item{coords}{Passed to \code{morie_spatial_variogram}.}
\item{values}{Passed to \code{morie_spatial_variogram}.}
\item{model}{Passed to \code{.morie_vgm_gamma}. Defaults to \code{"exponential"}.}
}
\description{
(Cressie 1985 weights n_h / h^2) -- the gstat::fit.variogram analogue. Milliseconds at any n; the full Gaussian-likelihood MLE stays available via morie_spatial_variogram_fit().
}
\value{
A list with \code{model}, \code{nugget}, \code{psill}, \code{range}, \code{method}.
}
\keyword{internal}
