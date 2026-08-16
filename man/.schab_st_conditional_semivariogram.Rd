% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_conditional_semivariogram}
\alias{.schab_st_conditional_semivariogram}
\title{.schab_st_conditional_semivariogram}
\usage{
.schab_st_conditional_semivariogram(coords, times, z, at_time, n_bins = 10L, max_dist = NULL, tol = 0)
}
\arguments{
\item{coords}{A matrix; indexed by row and column.}
\item{times}{Numeric; combined arithmetically in the body.}
\item{z}{A vector; indexed elementwise.}
\item{at_time}{See Usage.}
\item{n_bins}{Defaults to \code{10L}.}
\item{max_dist}{Defaults to \code{NULL}.}
\item{tol}{Defaults to \code{0}.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{spstvg}. See the file header for the source the module follows.
}
\value{
A list with \code{gamma}, \code{counts}, \code{n_at_time}, \code{lags}, \code{edges}.
}
\keyword{internal}
