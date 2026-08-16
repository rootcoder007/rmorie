% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sv_bayes_unfold}
\alias{.morie_sv_bayes_unfold}
\title{Internal helper: native Bayesian unfolding sampler}
\usage{
.morie_sv_bayes_unfold(P, n_dims = 2L, n_samples = 1000L, burn_in = 200L)
}
\description{
Internal helper: native Bayesian unfolding sampler
}
\value{
A list with \code{stimuli}, \code{ideal_points}, \code{acceptance}, \code{n_samples}, \code{engine}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_sv_bayes_unfold(M)
}
