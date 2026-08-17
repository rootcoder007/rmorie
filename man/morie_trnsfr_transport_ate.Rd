% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trnsfr_transport_ate}
\alias{morie_trnsfr_transport_ate}
\title{morie_trnsfr_transport_ate}
\usage{
morie_trnsfr_transport_ate(Y, W, X, S, method = "dr", e = NULL, trim = 0.001, ridge = 1e-06)
}
\arguments{
\item{Y}{Passed to \code{.s03vec}.}
\item{W}{Passed to \code{.s03vec}.}
\item{X}{Passed to \code{.s03mat}.}
\item{S}{Passed to \code{.trnsfr_cohort}.}
\item{method}{One of \code{"balance"}, \code{"ipw"}, \code{"outcome"}. Defaults to \code{"dr"}.}
\item{e}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{trim}{Passed to \code{morie_trnsfr_transport_weights}. Defaults to \code{0.001}.}
\item{ridge}{Passed to \code{morie_trnsfr_transport_weights}. Defaults to \code{1e-06}.}
}
\description{
A step of the trnsfr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{source_ate}, \code{outcome_route}, \code{n_source}, \code{n_target}, \code{method}, \code{diagnostics}, \code{assumption}.
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
set.seed(2)
n <- 100
X <- matrix(rnorm(n * 2), n, 2)
S <- rbinom(n, 1, plogis(0.3 * X[, 1]))
W <- rbinom(n, 1, 0.5)
Y <- 1 + 0.8 * W + X[, 1] + rnorm(n, 0, 0.3)
r <- morie_trnsfr_transport_ate(Y, W, X, S)
str(r, max.level = 1)
}
