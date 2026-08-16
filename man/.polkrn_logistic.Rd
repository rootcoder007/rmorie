% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.polkrn_logistic}
\alias{.polkrn_logistic}
\title{Iteratively reweighted least squares for logistic regression}
\usage{
.polkrn_logistic(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
}
\description{
X: design matrix (with intercept), y: 0/1 vector.
}
\value{
A list with \code{beta}, \code{mu}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.polkrn_logistic(V, V)
}
