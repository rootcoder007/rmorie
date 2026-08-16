% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_periodogram_from_cov}
\alias{.schab_periodogram_from_cov}
\title{.schab_periodogram_from_cov}
\usage{
.schab_periodogram_from_cov(z)
}
\arguments{
\item{z}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{spperiod}. See the file header for the source the module follows.
}
\value{
A list with \code{periodogram}, \code{omega1}, \code{omega2}, \code{covariance}, \code{lags_j}, \code{lags_k}.
}
\keyword{internal}
