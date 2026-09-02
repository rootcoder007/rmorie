% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_svycox}
\alias{morie_svycox}
\title{morie_svycox}
\usage{
morie_svycox(time, event, X, weights = NULL, strata = NULL, cluster = NULL, 
    ...) 
}
\arguments{
\item{time}{Passed to \code{.svycox_svycoxph}.}
\item{event}{Passed to \code{.svycox_svycoxph}.}
\item{X}{Passed to \code{.svycox_svycoxph}.}
\item{weights}{Passed to \code{.svycox_svycoxph}.}
\item{strata}{Passed to \code{.svycox_svycoxph}.}
\item{cluster}{Passed to \code{.svycox_svycoxph}.}
\item{...}{Passed through.}
}
\description{
A step of the svycox_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.svycox_svycoxph}.
}
\references{
----------
Binder, D. A. (1992) "Fitting Cox\'s proportional hazards models from
survey data", *Biometrika* 79(1), 139-147,
doi:10.1093/biomet/79.1.139. The weighted estimating equation above,
the interpretation of \eqn{\hat\beta} as a finite-population census
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
morie_svycox(time = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8))
}
