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
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{z}{A vector; its length is taken and its elements indexed.}
\item{targets}{A matrix; passed to \code{as.matrix}.}
\item{min_sites}{Passed to \code{.schab_haas_window}. Defaults to \code{35L}.}
\item{step}{Passed to \code{.schab_haas_window}. Defaults to \code{5L}.}
\item{n_lags}{Passed to \code{.schab_empirical_variogram}. Defaults to \code{10L}.}
\item{local_mean}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{local_variogram}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{spmwst}. See the file header for the source the module follows.
}
\value{
A list with \code{prediction}, \code{local_sill}, \code{local_range}, \code{window_sizes}, \code{converged}, \code{theta_is_global}, \code{global_sill}, \code{global_range}, \code{caveats}.
}
\keyword{internal}
