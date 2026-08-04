# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean and variance of the smoothed sign test statistic
#'
#' Sec. 5.3.1. With `Stilde = n - sum_i K(-X_i/h)`,
#' \deqn{E_\theta(\tilde S) = n\{F(\theta) + O(h^2)\},\quad V_\theta(\tilde S) = n[\{1-F(\theta)\}F(\theta) + O(h)].}{E(Stilde) = n{F(theta) + O(h^2)}, V(Stilde) = n[{1 - F(theta)} F(theta) + O(h)].}
#'
#' Under `H0` these reduce to `n/2` and `n/4`, and Theorem 5.10 refines the
#' variance to `n/4 - 2 n h f(0) A11 - (n h^3 / 3) f''(0) A13 + o(1)` with
#' `A_{i,j} = int K^i(u) k(u) u^j du`. Pass `a11` and `a13` with `f0`/`fpp0`
#' for that refinement; otherwise the leading forms are returned and `refined`
#' is `FALSE`.
#'
#' The point is in the error terms. The ordinary sign test `S` is discrete, so
#' its standardised version jumps by `O(n^-1/2)` and no Edgeworth expansion can
#' be valid for it. `Stilde` is continuous, and under `H0` its leading moments
#' do not depend on `F` at all -- asymptotically distribution-free, which makes
#' Theorem 5.9 possible.
#'
#' This module previously carried a copy of a Kolmogorov-Smirnov
#' implementation, returning a KS statistic under the name of the sign test's
#' moments. It now computes the moments.
#'
#' Verified against Maesono, Moriyama and Lu (2018), AISM 70(5):969-982
#' (arXiv:1610.02145), Sec. 3.
#'
#' @param n Sample size.
#' @param ftheta `F(theta)`; 1/2 under the null.
#' @param h Bandwidth, needed for the Theorem 5.10 refinement.
#' @param f0,fpp0 `f(0)` and `f''(0)`.
#' @param a11,a13 `A_{1,1}` and `A_{1,3}`.
#' @return Named list with ``mean``, ``variance``, ``se``, ``refined``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Sec. 5.3.1 and Theorem 5.10; Maesono, Moriyama and Lu (2018), AISM 70:969-982.
#' @examples
#' Ssgnmom(n = 100)
#' @export
Ssgnmom <- function(n, ftheta = 0.5, h = NULL, f0 = NULL, fpp0 = NULL, a11 = NULL, a13 = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  if (ftheta < 0 || ftheta > 1) stop("F(theta) must lie in [0, 1].")
  mean <- n * ftheta
  v <- n * (1 - ftheta) * ftheta
  refined <- FALSE
  if (!is.null(h) && !is.null(f0) && !is.null(fpp0) && !is.null(a11) && !is.null(a13)) {
    if (h <= 0) stop("bandwidth must be positive.")
    v <- n / 4 - 2 * n * h * f0 * a11 - n * h^3 / 3 * fpp0 * a13
    refined <- TRUE
  }
  list(mean = mean, variance = v, se = if (v > 0) sqrt(v) else NA_real_,
       refined = refined, n = n,
       method = "smoothed sign test mean and variance (Sec. 5.3.1)")
}

# CANONICAL TEST
# r <- Ssgnmom(n = 100); stopifnot(r$mean == 50, r$variance == 25)

#' @rdname Ssgnmom
#' @keywords internal
#' @export
morie_fauzi_sign_moments <- Ssgnmom
