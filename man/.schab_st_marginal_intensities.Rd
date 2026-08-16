% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_marginal_intensities}
\alias{.schab_st_marginal_intensities}
\title{.schab_st_marginal_intensities}
\usage{
.schab_st_marginal_intensities(points, times, region, time_interval, n_space_bins = 4L, n_time_bins = 4L)
}
\arguments{
\item{points}{See Usage.}
\item{times}{See Usage.}
\item{region}{See Usage.}
\item{time_interval}{See Usage.}
\item{n_space_bins}{Defaults to \code{4L}.}
\item{n_time_bins}{Defaults to \code{4L}.}
}
\description{
Part of the schab_st_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{marginal_spatial}, \code{marginal_temporal}, \code{cell_area}, \code{bin_width}, \code{x_edges}, \code{y_edges}, \code{t_edges}.
}
\keyword{internal}
