% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_pairwise_distances}
\alias{.schab_pairwise_distances}
\title{.schab_pairwise_distances}
\usage{
.schab_pairwise_distances(coords)
}
\arguments{
\item{coords}{A matrix; passed to \code{ncol}.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_mgwr_backfit}, \code{.schab_select_bandwidth}, \code{spgwrb}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{as.matrix}.
}
\keyword{internal}
