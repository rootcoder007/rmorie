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
\item{time}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{event}{Coerced to integer by the body, with \code{as.integer}.}
\item{X}{A matrix; indexed by row and column.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{strata}{Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.}
\item{cluster}{Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the svycox_native implementation. Called by \code{.svycox_score_residuals}, \code{.svycox_svycoxph}. See the file header for the source the module follows.
}
\value{
A list with \code{T}, \code{E}, \code{M}, \code{w}, \code{h}, \code{c}, \code{n}, \code{p}.
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
\examples{
.svycox_prep(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8), strata = c(1, 2, 3, 4, 5, 6, 7, 8), cluster = c(1, 2, 3, 4, 5, 6, 7, 8))
}
