% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_lag_matrices}
\alias{.schab_st_lag_matrices}
\title{.schab_st_lag_matrices}
\usage{
.schab_st_lag_matrices(coords, times)
}
\arguments{
\item{coords}{A matrix; passed to \code{nrow}.}
\item{times}{A vector; its length is taken.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{.schab_st_covariance_matrix}. See the file header for the source the module follows.
}
\value{
A list with \code{d}, \code{k}.
}
\keyword{internal}
