% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_empirical_semivariogram}
\alias{.schab_st_empirical_semivariogram}
\title{.schab_st_empirical_semivariogram}
\usage{
.schab_st_empirical_semivariogram(coords, times, z, n_space_bins = 10L, n_time_bins = 5L, max_dist = NULL, max_time = NULL)
}
\arguments{
\item{coords}{See Usage.}
\item{times}{See Usage.}
\item{z}{See Usage.}
\item{n_space_bins}{Defaults to \code{10L}.}
\item{n_time_bins}{Defaults to \code{5L}.}
\item{max_dist}{Defaults to \code{NULL}.}
\item{max_time}{Defaults to \code{NULL}.}
}
\description{
Part of the schab_st_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{gamma}, \code{counts}, \code{space_lags}, \code{time_lags}, \code{space_edges}, \code{time_edges}.
}
\keyword{internal}
