# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kullback-Leibler divergence with additive smoothing for sparse counts
#'
#' Additive (Lidstone) smoothing replaces a maximum-likelihood count
#' ratio by (c + eps)/(N + V eps), with V the vocabulary size.  Without
#' it the Kullback-Leibler divergence of two sparse count vectors is
#' infinite the moment the second assigns zero to an event the first
#' does not, which is the failure mode the smoothing exists to prevent.
#' Both vectors are smoothed with the same eps, so the divergence is
#' finite by construction.  The reverse divergence and their sum
#' (Kullback's symmetric divergence J) are reported too, since KL is not
#' symmetric.
#'
#' Formula: P = (c + eps)/(N + V eps); KL(P||Q) = sum P log(P/Q).
#'
#' @param p Non-negative counts over the vocabulary.
#' @param q Non-negative counts over the same vocabulary.
#' @param eps Additive smoothing constant, strictly positive.
#' @return List with \code{estimate}, \code{kl_pq}, \code{kl_qp},
#'   \code{symmetric_kl}, \code{eps}, \code{vocabulary}, \code{zeros_p},
#'   \code{zeros_q}, \code{mass_p}, \code{mass_q}, \code{n},
#'   \code{method}.
#' @references Chen and Goodman (1996), An empirical study of smoothing
#'   techniques for language modeling, Proceedings of the 34th Annual
#'   Meeting of the ACL, pp. 310-318, section 2.1.
#'   \doi{10.3115/981863.981904}
#' @export
Klmsm1 <- function(p, q, eps) {
  pv <- .s03vec(p); qv <- .s03vec(q); V <- length(pv)
  if (V == 0L) stop("kl_molecular_smooth: p is empty")
  if (length(qv) != V) stop("kl_molecular_smooth: p and q have different lengths")
  if (any(c(pv, qv) < 0)) stop("kl_molecular_smooth: counts must be non-negative")
  e <- as.numeric(eps)
  if (e <= 0) stop("kl_molecular_smooth: eps must be positive")
  P <- (pv + e) / (sum(pv) + V * e)
  Q <- (qv + e) / (sum(qv) + V * e)
  kl <- sum(P * log(P / Q)); rk <- sum(Q * log(Q / P))
  .t1_result(estimate = kl, kl_pq = kl, kl_qp = rk, symmetric_kl = kl + rk,
             eps = e, vocabulary = V, zeros_p = sum(pv == 0),
             zeros_q = sum(qv == 0), mass_p = sum(pv), mass_q = sum(qv), n = V,
             method = "P = (c + eps)/(N + V eps) then KL(P||Q) = sum P log(P/Q), Chen & Goodman (1996)")
}
