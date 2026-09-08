# SPDX-License-Identifier: AGPL-3.0-or-later
#' Minimum covariance determinant, by exhaustive enumeration
#'
#' Rousseeuw, P. J. (1985), "Multivariate estimation with high breakdown
#' point", in Mathematical Statistics and Applications, Vol. B, Reidel,
#' 283-297, where the MCD was introduced.  The definition used here is the one
#' restated by Hubert, Debruyne and Rousseeuw (2018), "Minimum Covariance
#' Determinant and Extensions", arXiv:1709.07045, section "Definition", which
#' was read directly: the raw MCD with tuning constant n/2 <= h <= n is
#' (mu0, S0) where mu0 is the mean of the h observations for which the
#' determinant of the sample covariance matrix is as small as possible, and S0
#' is that covariance matrix multiplied by a consistency factor c0.
#'
#' The same source gives c0 = alpha / F_chi2_\{p+2\}(q_alpha) with alpha = h/n
#' and q_alpha the alpha-quantile of the chi2_p distribution, gives the most
#' robust subset size h = \[(n + p + 1)/2\], and gives the requirement h > p,
#' since otherwise every h-subset has a singular covariance matrix.
#'
#' THIS function computes the estimator by its definition, enumerating every
#' h-subset.  That is exponential, and the same source says so plainly.  The
#' point of having it is that it is the ground truth the FastMCD approximation
#' in Fastm is checked against.  It refuses rather than silently approximating
#' when the enumeration would exceed max_subsets.
#'
#' The univariate case has a closed form the enumeration must reproduce: for
#' p = 1 the h-subset of smallest variance is CONTIGUOUS in the sorted sample.
#'
#' @param X n-by-p data matrix.
#' @param h subset size; defaults to \[(n + p + 1)/2\].
#' @param n_starts ignored; accepted so the signature matches Fastm.
#' @param max_subsets refuse rather than enumerate more than this many.
#' @return list: estimate, center, cov_raw, cov, factor, subset, h, n, p,
#'   n_subsets, method.
#' @keywords internal
#' @examples
#' Mcdv(cbind(c(1, 2, 3, 4, 20), c(1, 2, 3, 4, 20)), 4)$center
#' @export
Mcdv <- function(X, h = NULL, n_starts = NULL, max_subsets = 200000) {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  if (n == 0L) stop("mcd: X is empty")
  p <- ncol(Xm)
  if (p == 0L) stop("mcd: X has no columns")
  hh <- if (is.null(h)) .rsmcdh(n, p) else as.integer(h)
  if (hh <= p) stop("mcd: h must exceed p, otherwise every subset is singular")
  if (hh > n) stop("mcd: h cannot exceed the number of observations")
  total <- .rsnchoosek(n, hh)
  if (total > max_subsets) stop(sprintf("mcd: %d subsets exceeds max_subsets; use Fastm for the approximate algorithm", total))
  best_idx <- NULL
  best_det <- NULL
  for (idx in .rscombos(n, hh)) {
    mc <- .rsmeancov(Xm, idx)
    d <- .rscovdet(mc$S)
    if (is.null(best_det) || d < best_det) { best_det <- d
    best_idx <- idx }
  }
  mc <- .rsmeancov(Xm, best_idx)
  c0 <- .rsconsistency(hh, n, p)
  list(estimate = best_det, center = mc$mu, cov_raw = mc$S, cov = mc$S * c0,
       factor = c0, subset = as.numeric(best_idx - 1L), h = hh, n = n, p = p,
       n_subsets = total,
       method = "Rousseeuw (1985) MCD by exhaustive enumeration of all h-subsets; c0 = alpha / F_chi2_{p+2}(q_alpha)")
}
