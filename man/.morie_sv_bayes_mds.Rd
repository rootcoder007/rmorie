% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sv_bayes_mds}
\alias{.morie_sv_bayes_mds}
\title{Internal helper: native Bayesian MDS sampler}
\usage{
.morie_sv_bayes_mds(D, n_dims = 2L, n_samples = 1000L, burn_in = 200L, sigma_init = 1)
}
\description{
Internal helper: native Bayesian MDS sampler
}
\value{
A list with \code{positions}, \code{sigma}, \code{acceptance}, \code{n_samples}, \code{engine}.
}
\keyword{internal}
\examples{
Xt <- matrix(rnorm(20), 10, 2)
Dt <- as.matrix(stats::dist(Xt))
.morie_sv_bayes_mds(Dt, n_dims = 2L, n_samples = 200L, burn_in = 100L)
}
