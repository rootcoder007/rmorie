% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_variogram_fit}
\alias{morie_variogram_fit}
\title{morie_variogram_fit}
\usage{
morie_variogram_fit(coords, values, model = "exponential", n_bins = 15, max_dist = NULL) 
}
\description{
morie_variogram_fit
}
\arguments{
\item{coords}{Argument \code{coords}; see Usage.}
\item{values}{Argument \code{values}; see Usage.}
\item{model}{Argument \code{model}; see Usage.}
\item{n_bins}{Argument \code{n_bins}; see Usage.}
\item{max_dist}{Argument \code{max_dist}; see Usage.}
}
\value{
A list with \code{c0}, \code{c}, \code{a}, \code{sill}, \code{model}, \code{lag}, \code{gamma_hat}, \code{counts}, \code{fitted}, \code{wss}, \code{range_at_bound}, \code{grid_lo}, \code{grid_hi}, \code{n_bins_used}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_variogram_fit(V, V)
}
