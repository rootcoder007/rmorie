% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_as_lags}
\alias{.schab_st_as_lags}
\title{.schab_st_as_lags}
\usage{
.schab_st_as_lags(h, k)
}
\arguments{
\item{h}{A vector; its length is taken.}
\item{k}{A vector; its length is taken.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{.schab_st_anisotropic_correlation}, \code{.schab_st_exponential_separable}, \code{.schab_st_gneiting} and 5 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{k}.
}
\keyword{internal}
