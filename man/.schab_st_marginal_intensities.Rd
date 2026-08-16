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
\item{points}{A matrix; passed to \code{as.matrix}.}
\item{times}{See Usage.}
\item{region}{Passed to \code{.schab_st_region_box}.}
\item{time_interval}{A vector; indexed elementwise.}
\item{n_space_bins}{Defaults to \code{4L}.}
\item{n_time_bins}{Defaults to \code{4L}.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{spstp}. See the file header for the source the module follows.
}
\value{
A list with \code{marginal_spatial}, \code{marginal_temporal}, \code{cell_area}, \code{bin_width}, \code{x_edges}, \code{y_edges}, \code{t_edges}.
}
\keyword{internal}
