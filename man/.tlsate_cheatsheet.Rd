% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlsate_cheatsheet}
\alias{.tlsate_cheatsheet}
\title{.tlsate_cheatsheet}
\usage{
.tlsate_cheatsheet()
}
\description{
A step of the tlsate_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A character value.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 12 (in
cluster randomized trials the units are not a simple random sample and
the target population is hypothetical or ill-defined, so the PATE may
be neither well defined nor easily interpretable; the SATE as the
mean difference in counterfactual outcomes for the study units; that
the SATE is not formally identifiable in finite samples but a TMLE
for the population effect is consistent and asymptotically linear
for it with an asymptotically conservative variance estimator; the
conservative influence curve dropping the Q1 - Q0 - psi term; the
extension to pair-matched trials; and the finding that with effect
modification, targeting the sample effect yields the most precision
and power).

Balzer, L. B., Petersen, M. L. & van der Laan, M. J. (2016) "Targeted
estimation and inference for the sample average treatment effect in
trials with and without pair-matching", *Statistics in Medicine*
35(21), 3717-3732, doi:10.1002/sim.6965.

Imbens, G. W. (2004) "Nonparametric estimation of average treatment
effects under exogeneity: a review", *Review of Economics and
Statistics* 86(1), 4-29, doi:10.1162/003465304323023651. The
sample/population distinction.
}
\keyword{internal}
