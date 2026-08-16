% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_scale_mixture}
\alias{.schab_st_scale_mixture}
\title{.schab_st_scale_mixture}
\usage{
.schab_st_scale_mixture(h, k, cov_spatial, cov_temporal, nodes, weights)
}
\arguments{
\item{h}{Passed to \code{.schab_st_as_lags}.}
\item{k}{Passed to \code{.schab_st_as_lags}.}
\item{cov_spatial}{Accepted by the signature and not used anywhere in the body.}
\item{cov_temporal}{Accepted by the signature and not used anywhere in the body.}
\item{nodes}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{weights}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{spstcn}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
