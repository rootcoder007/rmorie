% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trnsfr_transport_weights}
\alias{morie_trnsfr_transport_weights}
\title{Odds-of-membership weights for the source cohort. Fits pi(x) by}
\usage{
morie_trnsfr_transport_weights(X, S, trim = 0.001, ridge = 1e-06)
}
\arguments{
\item{X}{Passed to \code{.s03mat}.}
\item{S}{Passed to \code{.trnsfr_cohort}.}
\item{trim}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.}
\item{ridge}{Passed to \code{.s03logit}. Defaults to \code{1e-06}.}
}
\description{
logistic regression on the pooled data and returns, for each source unit, (1-pi)/pi, normalised to mean 1. Target units get 0.
}
\value{
A list with \code{weights}, \code{pi}, \code{max_weight}, \code{ess}, \code{ess_fraction}, \code{n_source}, \code{coef}, \code{method}.
}
\references{
----------
Wager, S. (2025) Causal Inference: A Statistical Learning Approach,
Stanford University, draft of 26 November 2025. Chapters 2 (Sec.
2.2), 3, and 7 (Secs. 7.1-7.2).

Notes: the ledger recorded this as "Athey-Wager (2019),
transfer-learning MSM across cohorts"; no paper of that title was
located and the implementation follows Wager (2025).
}
\keyword{internal}
\examples{
set.seed(1)
n <- 60
X <- matrix(rnorm(n * 2), n, 2)
S <- rbinom(n, 1, plogis(0.3 * X[, 1]))
r <- morie_trnsfr_transport_weights(X, S)
str(r, max.level = 1)
}
