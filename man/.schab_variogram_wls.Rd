% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_variogram_wls}
\alias{.schab_variogram_wls}
\title{.schab_variogram_wls}
\usage{
.schab_variogram_wls(h, gamma, counts)
}
\arguments{
\item{h}{A vector; indexed elementwise.}
\item{gamma}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{counts}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}. See the file header for the source the module follows.
}
\value{
A list with \code{sill}, \code{range}, \code{converged}, \code{wls}.
}
\keyword{internal}
