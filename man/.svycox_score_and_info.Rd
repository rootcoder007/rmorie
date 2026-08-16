% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svycox_score_and_info}
\alias{.svycox_score_and_info}
\title{.svycox_score_and_info}
\usage{
.svycox_score_and_info(T, E, M, w, beta, n, p)
}
\arguments{
\item{T}{Numeric; passed to \code{order}.}
\item{E}{A vector; indexed elementwise.}
\item{M}{A vector; indexed elementwise.}
\item{w}{A vector; indexed elementwise.}
\item{beta}{Numeric; combined arithmetically in the body.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the svycox_native implementation. Called by \code{.svycox_score_residuals}, \code{.svycox_svycoxph}. See the file header for the source the module follows.
}
\value{
A list with \code{U}, \code{I}, \code{resid}.
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
