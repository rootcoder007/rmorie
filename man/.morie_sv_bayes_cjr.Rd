% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sv_bayes_cjr}
\alias{.morie_sv_bayes_cjr}
\title{Internal helper: native CJR IRT sampler}
\usage{
.morie_sv_bayes_cjr(votes, n_samples = 1000L, burn_in = 200L)
}
\description{
Internal helper: native CJR IRT sampler
}
\value{
A list with \code{ideal_points}, \code{ideal_sd}, \code{discrimination}, \code{difficulty}, \code{n_samples}, \code{engine}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_sv_bayes_cjr(V)
}
