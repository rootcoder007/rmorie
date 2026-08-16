% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.polkrn_wls}
\alias{.polkrn_wls}
\title{Weighted least squares with an intercept added automatically}
\usage{
.polkrn_wls(X, y, w)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
Returns a list with coef, se, vcov.
}
\value{
A list with \code{coef}, \code{se}, \code{vcov}.
}
\references{
----------
Hernan, M. A., Brumback, B. & Robins, J. M. (2002) "Estimating the
causal effect of zidovudine on CD4 count with a marginal structural
model for repeated measures", Statistics in Medicine 21(12),
1689-1709, doi:10.1002/sim.1144.

Robins, J. M., Hernan, M. A. & Brumback, B. (2000) "Marginal structural
models and causal inference in epidemiology", Epidemiology 11(5),
550-560, doi:10.1097/00001648-200009000-00011 -- the weights.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
Chapman & Hall/CRC, Sec. 21.2.
}
\keyword{internal}
\examples{
.polkrn_wls(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
