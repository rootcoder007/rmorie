% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tloilr_constrained_rule}
\alias{.tloilr_constrained_rule}
\title{.tloilr_constrained_rule}
\usage{
.tloilr_constrained_rule(B, kappa)
}
\arguments{
\item{B}{See Usage.}
\item{kappa}{See Usage.}
}
\description{
Part of the tloilr_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{rule}, \code{tau}, \code{treated_fraction}, \code{binding}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 23
(Luedtke & van der Laan): a resource constraint under which a maximum
proportion of the population can be treated; a root-n rate estimator
for the optimal resource-constrained value with confidence intervals;
efficiency among all regular asymptotically linear estimators in the
nonparametric model; and the statement that when the baseline
covariates are continuous and the resource constraint is ACTIVE --
the constrained value strictly below the unconstrained one -- the
conditions are more reasonable than the nonexceptional law assumption
needed for regular estimation of the optimal unconstrained value in
Chap. 22. The data structure (W, A, Y) with Y bounded in the unit
interval, noting any bounded continuous outcome can be rescaled.

Luedtke, A. R. & van der Laan, M. J. (2016) "Optimal Individualized
Treatments in Resource-Limited Settings", International Journal of
Biostatistics 12(1), 283-303, doi:10.1515/ijb-2015-0007.

Luedtke, A. R. & van der Laan, M. J. (2016) "Statistical inference
for the mean outcome under a possibly non-unique optimal treatment
strategy", Annals of Statistics 44(2), 713-742, doi:10.1214/15-AOS1384.
The nonexceptional law condition.
}
\keyword{internal}
