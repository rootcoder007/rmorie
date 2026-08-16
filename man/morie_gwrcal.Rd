% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwrcal}
\alias{morie_gwrcal}
\title{GWR bandwidth selection}
\usage{
morie_gwrcal(y, X, coords, kernel = "gaussian", criterion = "aicc", adaptive = FALSE, bounds = NULL, search = NULL, n_points = 30L, tol = 1e-04)
}
\arguments{
\item{y,X,coords}{Response, design, locations.}
\item{kernel}{One of \code{"gaussian"}, \code{"bisquare"},   \code{"tricube"}, \code{"boxcar"}.}
\item{criterion}{One of \code{"aicc"}, \code{"cv"}, \code{"aic"}.}
\item{adaptive}{Treat the bandwidth as a neighbour count.}
\item{bounds}{Optional \code{c(lo, hi)} override.}
\item{search}{One of \code{"golden"}, \code{"grid"}.}
\item{n_points}{Number of points for the grid.}
}
\description{
Minimises CV, AIC or AICc over a bandwidth grid. AICc is the default: the small-sample correction, and GWR\'s whole point is that local samples are small.
}
\value{
A list with \code{bandwidth}, \code{score},   \code{criterion}, \code{kernel}, \code{adaptive}, \code{search},   \code{grid}, \code{profile}, \code{at_boundary},   \code{coefficients}, \code{fitted}, \code{residuals},   \code{tr_S}, \code{sigma2}, \code{aicc}, \code{r_squared},   \code{ols_aicc}, \code{aicc_improvement}, \code{method},   \code{note}.
}
\keyword{internal}
