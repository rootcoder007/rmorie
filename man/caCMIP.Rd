% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{caCMIP}
\alias{caCMIP}
\title{caCMIP}
\usage{
caCMIP(models, obs, sigma_d, sigma_s, projections = NULL)
}
\arguments{
\item{models}{See Usage.}
\item{obs}{See Usage.}
\item{sigma_d}{See Usage.}
\item{sigma_s}{See Usage.}
\item{projections}{Defaults to \code{NULL}.}
}
\description{
Part of the caCMIP_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{weights}, \code{unweighted_mean}, \code{d}, \code{n_models}, \code{effective_n}, \code{method}.
}
\keyword{internal}
