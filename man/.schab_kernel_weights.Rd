% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_kernel_weights}
\alias{.schab_kernel_weights}
\title{.schab_kernel_weights}
\usage{
.schab_kernel_weights(distance, bandwidth, kernel = "gaussian", normalized = FALSE)
}
\arguments{
\item{distance}{Passed to \code{.schab_reshape_like}.}
\item{bandwidth}{See Usage.}
\item{kernel}{Compared against \code{"gaussian"}. Defaults to \code{"gaussian"}.}
\item{normalized}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_local_weights}, \code{spgwrk}. See the file header for the source the module follows.
}
\value{
The value of \code{.schab_reshape_like}.
}
\keyword{internal}
