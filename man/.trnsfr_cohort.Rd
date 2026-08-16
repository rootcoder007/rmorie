% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trnsfr_cohort}
\alias{.trnsfr_cohort}
\title{.trnsfr_cohort}
\usage{
.trnsfr_cohort(S)
}
\arguments{
\item{S}{Passed to \code{.s03vec}.}
}
\description{
A step of the trnsfr_native implementation. Called by \code{morie_trnsfr_balancing_weights}, \code{morie_trnsfr_transport_ate}, \code{morie_trnsfr_transport_weights}. See the file header for the source the module follows.
}
\value{
The value of \code{s}, as built in the body.
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
