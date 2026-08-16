% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svycox_score_residuals}
\alias{.svycox_score_residuals}
\title{.svycox_score_residuals}
\usage{
.svycox_score_residuals(time, event, X, beta, weights = NULL)
}
\arguments{
\item{time}{Passed to \code{.svycox_prep}.}
\item{event}{Passed to \code{.svycox_prep}.}
\item{X}{Passed to \code{.svycox_prep}.}
\item{beta}{Passed to \code{.svycox_score_and_info}.}
\item{weights}{Passed to \code{.svycox_prep}.}
}
\description{
A step of the svycox_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
}
\references{
----------
Binder, D. A. (1992) "Fitting Cox\'s proportional hazards models from
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
