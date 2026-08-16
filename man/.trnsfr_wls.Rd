% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trnsfr_wls}
\alias{.trnsfr_wls}
\title{Weighted least squares with an intercept prepended. rows is a}
\usage{
.trnsfr_wls(rows, y, w)
}
\arguments{
\item{rows}{See Usage.}
\item{y}{See Usage.}
\item{w}{See Usage.}
}
\description{
matrix (or vector) of predictors; returns list(coef=...) with the intercept first.
}
\value{
A list with \code{coef}.
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
