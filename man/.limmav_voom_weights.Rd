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
\item{counts}{Passed to \code{.limmav_log_cpm}.}
\item{design}{A matrix; passed to \code{as.matrix}.}
\item{lib_sizes}{Passed to \code{.limmav_log_cpm}.}
\item{span}{Passed to \code{.limmav_lowess}. Defaults to \code{0.5}.}
}
\description{
A step of the limmav_native implementation. Called by \code{morie_limmav}. See the file header for the source the module follows.
}
\value{
A list with \code{log_cpm}, \code{weights}, \code{mean_log_count}, \code{sqrt_sd}, \code{trend_x}, \code{trend_y}, \code{lib_sizes}, \code{lo}.
}
\keyword{internal}
