% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_covariance_matrix}
\alias{.schab_st_covariance_matrix}
\title{.schab_st_covariance_matrix}
\usage{
.schab_st_covariance_matrix(coords, times, cov_fn)
}
\arguments{
\item{coords}{Passed to \code{.schab_st_lag_matrices}.}
\item{times}{Passed to \code{.schab_st_lag_matrices}.}
\item{cov_fn}{See Usage.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{.schab_st_is_valid_covariance}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
