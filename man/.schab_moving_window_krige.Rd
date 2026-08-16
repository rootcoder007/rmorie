% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_moving_window_krige}
\alias{.schab_moving_window_krige}
\title{.schab_moving_window_krige}
\usage{
.schab_moving_window_krige(coords, z, targets, min_sites = 35L, step = 5L, n_lags = 10L, local_mean = FALSE, local_variogram = TRUE)
}
\arguments{
\item{coords}{See Usage.}
\item{z}{See Usage.}
\item{targets}{See Usage.}
\item{min_sites}{Defaults to \code{35L}.}
\item{step}{Defaults to \code{5L}.}
\item{n_lags}{Defaults to \code{10L}.}
\item{local_mean}{Defaults to \code{FALSE}.}
\item{local_variogram}{Defaults to \code{TRUE}.}
}
\description{
Part of the schab_rest_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{prediction}, \code{local_sill}, \code{local_range}, \code{window_sizes}, \code{converged}, \code{theta_is_global}, \code{global_sill}, \code{global_range}, \code{caveats}.
}
\keyword{internal}
