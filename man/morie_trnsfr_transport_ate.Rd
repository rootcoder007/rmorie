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
\item{Y}{See Usage.}
\item{W}{See Usage.}
\item{X}{See Usage.}
\item{S}{See Usage.}
\item{method}{Defaults to \code{"dr"}.}
\item{e}{Defaults to \code{NULL}.}
\item{trim}{Defaults to \code{0.001}.}
\item{ridge}{Defaults to \code{1e-06}.}
}
\description{
Part of the trnsfr_native implementation; see the file header for the source it follows.
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
