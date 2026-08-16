% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sv_bayes_am}
\alias{.morie_sv_bayes_am}
\title{Internal helper: native Bayesian Aldrich-McKelvey sampler}
\usage{
.morie_sv_bayes_am(Z, n_samples = 1000L, burn_in = 200L, prior_sd = 10)
}
\description{
Internal helper: native Bayesian Aldrich-McKelvey sampler
}
\value{
A list with \code{zeta_mean}, \code{zeta_sd}, \code{sigma2}, \code{n_samples}, \code{engine}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_sv_bayes_am(M)
}
