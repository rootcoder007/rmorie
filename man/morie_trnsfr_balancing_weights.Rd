% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trnsfr_balancing_weights}
\alias{morie_trnsfr_balancing_weights}
\title{Minimum-variance weights that match the target\'s X means: solve}
\usage{
morie_trnsfr_balancing_weights(X, S, ridge = 1e-08) 
}
\arguments{
\item{X}{Passed to \code{.s03mat}.}
\item{S}{Passed to \code{.trnsfr_cohort}.}
\item{ridge}{A matrix; passed to \code{diag}. Defaults to \code{1e-08}.}
}
\description{
min sum w_i^2 over source units subject to sum w_i Xtilde_i = Xbar_target with Xtilde = (1, X). Weights may go negative, which is reported.
}
\value{
A list with \code{weights}, \code{target_moments}, \code{achieved}, \code{max_imbalance}, \code{n_negative}, \code{positive_mass}, \code{method}.
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
r <- morie_trnsfr_balancing_weights(X, S)
str(r, max.level = 1)
}
