% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_sample_cov2d}
\alias{.schab_sample_cov2d}
\title{.schab_sample_cov2d}
\usage{
.schab_sample_cov2d(z)
}
\arguments{
\item{z}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_periodogram_from_cov}, \code{spperiod}. See the file header for the source the module follows.
}
\value{
A list with \code{cov}, \code{lags_j}, \code{lags_k}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.schab_sample_cov2d(M)
}
