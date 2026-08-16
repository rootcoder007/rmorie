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
\item{coords}{A matrix; indexed by row and column.}
\item{times}{A vector; its length is taken and its elements indexed.}
\item{z}{A vector; its length is taken and its elements indexed.}
\item{n_space_bins}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10L}.}
\item{n_time_bins}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{max_dist}{The body requires: `max_dist` and `max_time` must be positive.}
\item{max_time}{The body requires: `max_dist` and `max_time` must be positive.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{spstvg}. See the file header for the source the module follows.
}
\value{
A list with \code{gamma}, \code{counts}, \code{space_lags}, \code{time_lags}, \code{space_edges}, \code{time_edges}.
}
\keyword{internal}
\examples{
.schab_st_empirical_semivariogram(coords = c(1, 2, 3, 4, 5, 6, 7, 8), times = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
