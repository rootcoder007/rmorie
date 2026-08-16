% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_local_weights}
\alias{.schab_local_weights}
\title{.schab_local_weights}
\usage{
.schab_local_weights(d_row, bandwidth, kernel, adaptive)
}
\arguments{
\item{d_row}{Passed to \code{.schab_adaptive_bandwidth}.}
\item{bandwidth}{Passed to \code{.schab_adaptive_bandwidth}.}
\item{kernel}{Passed to \code{.schab_kernel_weights}.}
\item{adaptive}{A flag; the body branches on it.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_cv_score}, \code{.schab_gwr_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{.schab_kernel_weights}.
}
\keyword{internal}
