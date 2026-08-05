# SPDX-License-Identifier: AGPL-3.0-or-later
#' FAST-MCD, the C-step algorithm for the minimum covariance determinant
#'
#' Rousseeuw, P. J. and Van Driessen, K. (1999), "A fast algorithm for the
#' minimum covariance determinant estimator", Technometrics 41(3), 212-223.
#' The algorithm and its key theorem were read from Hubert, Debruyne and
#' Rousseeuw (2018), "Minimum Covariance Determinant and Extensions",
#' arXiv:1709.07045, section "COMPUTATION", which restates them verbatim:
#' take H1 of size h, put mu1 and S1 the empirical mean and covariance of the
#' data in H1; if |S1| is not 0 define d1(i) = d(x_i, mu1, S1); take H2 to be
#' the h observations with the smallest distances and compute mu2, S2 on H2;
#' then |S2| <= |S1|, with equality if and only if mu2 = mu1 and S2 = S1.
#'
#' That is the C-step, C for concentration.  The determinant never increases
#' along the iteration, which is what makes it terminate, and that
#' monotonicity is asserted directly as an anchor rather than taken on trust.
#'
#' The published schedule, same section: apply only TWO C-steps to each initial
#' subset, keep the ten with the lowest determinants, and iterate only those to
#' convergence.  Initial subsets are grown from elemental (p+1)-subsets.
#'
#' DETERMINISM.  The paper draws its elemental subsets at random.  This
#' implementation enumerates them in lexicographic order and takes the first
#' n_starts, so both language arms visit the same candidates in the same
#' sequence and return the same numbers, not merely numbers of equal quality.
#'
#' @param X n-by-p data matrix.
#' @param h subset size; defaults to [(n + p + 1)/2].
#' @param n_starts how many lexicographic elemental subsets to start from.
#' @param max_iter cap on the C-steps for each retained subset.
#' @param n_keep how many best subsets to iterate to convergence; the paper
#'   uses 10.
#' @return list: estimate, center, cov_raw, cov, factor, subset, dets,
#'   n_starts_used, h, n, p, method.
#' @keywords internal
#' @examples
#' Fastm(cbind(c(1, 2, 3, 4, 20), c(1, 2, 3, 4, 20)), 4)$estimate
#' @export
Fastm <- function(X, h = NULL, n_starts = 500L, max_iter = 100L, n_keep = 10L) {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  if (n == 0L) stop("fast_mcd: X is empty")
  p <- ncol(Xm)
  if (p == 0L) stop("fast_mcd: X has no columns")
  hh <- if (is.null(h)) .rsmcdh(n, p) else as.integer(h)
  if (hh <= p) stop("fast_mcd: h must exceed p, otherwise every subset is singular")
  if (hh > n) stop("fast_mcd: h cannot exceed the number of observations")
  if (n <= p + 1L) stop("fast_mcd: need more than p + 1 observations")
  seeds <- .rscombosstride(n, p + 1L, as.integer(n_starts))
  cdets <- numeric(0)
  cidx <- list()
  used <- 0L
  for (s in seeds) {
    mc <- .rsmeancov(Xm, s)
    dd <- .rsmahal2(Xm, mc$mu, mc$S)
    if (is.null(dd)) next
    idx <- sort(.rsosort(dd)[seq_len(hh)])
    used <- used + 1L
    for (q in 1:2) {
      st <- .rscstep(Xm, idx, hh)
      if (is.null(st)) break
      idx <- st$idx
    }
    mc <- .rsmeancov(Xm, idx)
    cdets <- c(cdets, .rscovdet(mc$S))
    cidx[[length(cidx) + 1L]] <- idx
  }
  if (length(cidx) == 0L) stop("fast_mcd: every elemental subset was degenerate")
  ord <- .rsosort(cdets)
  keep <- cidx[ord[seq_len(min(as.integer(n_keep), length(ord)))]]
  best_idx <- NULL; best_det <- NULL; best_chain <- numeric(0)
  for (idx in keep) {
    chain <- numeric(0)
    for (q in seq_len(as.integer(max_iter))) {
      mc <- .rsmeancov(Xm, idx)
      chain <- c(chain, .rscovdet(mc$S))
      st <- .rscstep(Xm, idx, hh)
      if (is.null(st)) break
      if (identical(st$idx, idx)) break
      idx <- st$idx
    }
    mc <- .rsmeancov(Xm, idx)
    d <- .rscovdet(mc$S)
    chain <- c(chain, d)
    if (is.null(best_det) || d < best_det) { best_det <- d; best_idx <- idx; best_chain <- chain }
  }
  mc <- .rsmeancov(Xm, best_idx)
  c0 <- .rsconsistency(hh, n, p)
  list(estimate = best_det, center = mc$mu, cov_raw = mc$S, cov = mc$S * c0,
       factor = c0, subset = as.numeric(best_idx - 1L), dets = best_chain,
       n_starts_used = used, h = hh, n = n, p = p,
       method = "Rousseeuw-Van Driessen (1999) FAST-MCD, two C-steps per lexicographic elemental start, ten best iterated to convergence")
}
