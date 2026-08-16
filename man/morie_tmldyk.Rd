% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmldyk}
\alias{morie_tmldyk}
\title{Differentially private TMLE of the ATE}
\usage{
morie_tmldyk(y, D, X, epsilon = 1, g_min = 0.05, seed = 0, g = NULL, Q1 = NULL, Q0 = NULL)
}
\arguments{
\item{y}{Outcome vector in [0,1].}
\item{D}{Treatment indicator.}
\item{X}{Covariate matrix.}
\item{epsilon}{Privacy budget.}
\item{g_min}{Propensity truncation bound.}
\item{seed}{Seed for the shared generator.}
\item{g}{Optional propensity score.}
\item{Q1}{Optional potential-outcome regression under treatment.}
\item{Q0}{Optional potential-outcome regression under control.}
}
\description{
The propensity score is truncated at \code{g_min} -- which bounds the sensitivity and is therefore part of the privacy guarantee, not a numerical convenience.
}
\value{
A list with \code{estimate}, \code{psi},   \code{non_private_psi}, \code{sensitivity}, \code{epsilon},   \code{g_min}, \code{se_private}, \code{se_sampling}, \code{ci},   \code{width_ratio}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(4)
n <- 60
X <- matrix(rnorm(n * 2), n, 2)
D <- rbinom(n, 1, 0.5)
y <- plogis(-0.3 + 0.8 * D + 0.5 * X[, 1] + rnorm(n, 0, 0.5))
r <- morie_tmldyk(y, D, X, epsilon = 1)
str(r, max.level = 1)
}
