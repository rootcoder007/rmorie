% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trnsfr_transfer_msm}
\alias{morie_trnsfr_transfer_msm}
\title{morie_trnsfr_transfer_msm}
\usage{
morie_trnsfr_transfer_msm(Y, A, H, cohort, target = 0, e = NULL, trim = 0.001, ridge = 1e-06)
}
\arguments{
\item{Y}{Passed to \code{.s03vec}.}
\item{A}{Passed to \code{.s03vec}.}
\item{H}{Passed to \code{.s03mat}.}
\item{cohort}{Coerced to character by the body, with \code{as.character}.}
\item{target}{Coerced to character by the body, with \code{as.character}. Defaults to \code{0}.}
\item{e}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{trim}{Passed to \code{morie_trnsfr_transport_weights}. Defaults to \code{0.001}.}
\item{ridge}{Passed to \code{morie_trnsfr_transport_weights}. Defaults to \code{1e-06}.}
}
\description{
A step of the trnsfr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{intercept}, \code{coef}, \code{weights}, \code{transport_weights}, \code{msm_weights}, \code{target}, \code{cohorts}, \code{n}, \code{method}.
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
set.seed(3)
n <- 100
H <- matrix(rnorm(n * 2), n, 2)
A <- rbinom(n, 1, 0.5)
cohort <- rbinom(n, 1, plogis(0.3 * H[, 1]))
Y <- 1 + 0.8 * A + H[, 1] + rnorm(n, 0, 0.3)
r <- morie_trnsfr_transfer_msm(Y, A, H, cohort, target = 0)
str(r, max.level = 1)
}
