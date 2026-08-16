% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{calibrated_rerank}
\alias{calibrated_rerank}
\title{Greedy maximum-marginal-relevance re-ranking for calibration.}
\usage{
calibrated_rerank(scores, p_g_given_i, p_target, N = 10, lam = 0.5, metric = "kl", alpha = 0.01, rank_weights = NULL)
}
\arguments{
\item{scores}{Numeric vector of per-item relevance scores.}
\item{p_g_given_i}{Numeric matrix or list of rows with \eqn{p(g \mid i)}.}
\item{p_target}{Target distribution \eqn{p(g \mid u)}.}
\item{N}{Number of items to select.}
\item{lam}{Trade-off between accuracy (\eqn{1-\lambda}) and   calibration (\eqn{\lambda}).}
\item{metric}{Either \code{"kl"} (default) or \code{"hellinger"}.}
\item{alpha}{Smoothing weight for the KL target (ignored for the   Hellinger metric).}
\item{rank_weights}{Optional numeric vector of per-rank weights   for the recommended list; if \code{NULL} each chosen slot has   weight 1.}
}
\description{
Solves eq. (6) by greedy maximisation of \eqn{(1-\lambda)\,s(I) - \lambda\, C(p, q(I))}. The surrogate is submodular, so every prefix of the returned list is \eqn{(1 - 1/e)} optimal.
}
\value{
A named list mirroring the Python RichResult payload:   \code{estimate}, \code{ranking} (identical 0-based indices in   Python, 1-based indices in R), \code{objective_path},   \code{q}, \code{p_target}, \code{calibration},   \code{calibration_uncalibrated}, \code{score},   \code{score_uncalibrated}, \code{lambda}, \code{metric},   \code{N}, \code{guarantee}, \code{method}.
}
\keyword{internal}
