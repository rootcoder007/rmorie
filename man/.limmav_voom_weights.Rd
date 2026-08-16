% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.limmav_voom_weights}
\alias{.limmav_voom_weights}
\title{.limmav_voom_weights}
\usage{
.limmav_voom_weights(counts, design, lib_sizes = NULL, span = 0.5)
}
\arguments{
\item{counts}{See Usage.}
\item{design}{See Usage.}
\item{lib_sizes}{Defaults to \code{NULL}.}
\item{span}{Defaults to \code{0.5}.}
}
\description{
Part of the limmav_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{log_cpm}, \code{weights}, \code{mean_log_count}, \code{sqrt_sd}, \code{trend_x}, \code{trend_y}, \code{lib_sizes}, \code{lo}.
}
\keyword{internal}
