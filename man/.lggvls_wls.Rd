% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lggvls_wls}
\alias{.lggvls_wls}
\title{Weighted least squares, intercept added}
\usage{
.lggvls_wls(X, y, w)
}
\arguments{
\item{X}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{y}{A vector; its length is taken.}
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the lggvls_native implementation. Called by \code{morie_lggvls}. See the file header for the source the module follows.
}
\value{
A list with \code{coef}, \code{se}, \code{vcov}, \code{sigma2}.
}
\references{
----------
Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period -- application to control of
the healthy worker survivor effect", Mathematical Modelling 7(9-12),
1393-1512, doi:10.1016/0270-0255(86)90088-6.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC, Sec. 21.2 for the product-over-time weights
and Ch. 20 for why conditioning fails where weighting works.
}
\keyword{internal}
\examples{
.lggvls_wls(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
