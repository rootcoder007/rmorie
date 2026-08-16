% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svycox_prep}
\alias{.svycox_prep}
\title{.svycox_prep}
\usage{
.svycox_prep(time, event, X, weights, strata, cluster)
}
\arguments{
\item{time}{See Usage.}
\item{event}{See Usage.}
\item{X}{See Usage.}
\item{weights}{See Usage.}
\item{strata}{See Usage.}
\item{cluster}{See Usage.}
}
\description{
Part of the svycox_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{T}, \code{E}, \code{M}, \code{w}, \code{h}, \code{c}, \code{n}, \code{p}.
}
\references{
----------
Binder, D. A. (1992) "Fitting Cox\\'s proportional hazards models from
survey data", *Biometrika* 79(1), 139-147,
doi:10.1093/biomet/79.1.139. The weighted estimating equation above,
the interpretation of :math:`\hat\beta` as a finite-population census
parameter, and the sandwich variance with the design-based variance
of the weighted score residuals in the middle.

Cox, D. R. (1972) "Regression models and life-tables", *Journal of
the Royal Statistical Society: Series B* 34(2), 187-202,
doi:10.1111/j.2517-6161.1972.tb00899.x, for the partial likelihood
being weighted; see :mod:`morie.fn.coxph` for the unweighted fit this
must reduce to.
}
\keyword{internal}
