% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_haas_window}
\alias{.schab_haas_window}
\title{.schab_haas_window}
\usage{
.schab_haas_window(coords, target, min_sites = 35L, step = 5L, lag_classes = NULL, max_sites = NULL)
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{target}{See Usage.}
\item{min_sites}{Defaults to \code{35L}.}
\item{step}{Defaults to \code{5L}.}
\item{lag_classes}{Defaults to \code{NULL}.}
\item{max_sites}{Defaults to \code{NULL}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}. See the file header for the source the module follows.
}
\value{
The value of \code{repeat}.
}
\keyword{internal}
