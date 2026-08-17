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
\item{models}{Iterated over elementwise, with \code{lapply}.}
\item{obs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sigma_d}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sigma_s}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{projections}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
Part of the caCMIP_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{weights}, \code{unweighted_mean}, \code{d}, \code{n_models}, \code{effective_n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
models <- list(rnorm(10), rnorm(10), rnorm(10))
caCMIP(models, obs = rnorm(10), sigma_d = 1, sigma_s = 1)
}
