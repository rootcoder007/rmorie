# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean and variance of the smoothed Wilcoxon signed rank statistic
#'
#' Sec. 5.3.1. With `Wtilde = n(n+1)/2 - sum_{i<=j} K(-(X_i+X_j)/(2h))`,
#' \deqn{E_\theta(\tilde W) = \frac{n(n+1)}{2}\{G(\theta) + O(h^2)\},\quad
#' V_\theta(\tilde W) = n(n+1)^2\Big\{\int F^2(u+2\theta)f(u)du - G^2(\theta) +
#' O(h^2)\Big\},}{E(Wtilde) = n(n+1)/2 {G(theta) + O(h^2)}, V(Wtilde) = n(n+1)^2 {int
#' F^2(u + 2 theta) f(u) du - G(theta)^2 + O(h^2)},}
#' with `G` the half-sum distribution function.
#'
#' `Wtilde` is a U-statistic, which is why its variance has the `n(n+1)^2`
#' shape rather than the sign test's `n`: the leading term is `n` times the
#' variance of the FIRST PROJECTION, and each of the `choose(n+1, 2)` pairs
#' contributes through it.
#'
#' Under `H0` with symmetric `F`, `G(0) = 1/2` and the projection integral is
#' `1/3`, giving the familiar `n(n+1)^2/12`. Those null values are the defaults.
#'
#' Unlike the sign test, `Wtilde` is NOT distribution-free -- the book says so
#' plainly -- but its asymptotic moments under `H0` do not depend on `F`, which
#' is all Theorem 5.9 needs.
#'
#' This module previously carried a copy of a Kolmogorov-Smirnov
#' implementation. It now computes the moments.
#'
#' Verified against Maesono, Moriyama and Lu (2018), AISM 70(5):969-982
#' (arXiv:1610.02145), Sec. 3.
#'
#' @param n Sample size.
#' @param gtheta `G(theta)`; 1/2 under the null.
#' @param projint `int F^2(u + 2 theta) f(u) du`; 1/3 under the null.
#' @return Named list with ``mean``, ``variance``, ``se``, ``projvar``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Sec. 5.3.1; Maesono, Moriyama and Lu (2018),
#' AISM 70:969-982.
#' @examples
#' Swilmom(n = 10)
#' @export
Swilmom <- function(n, gtheta = 0.5, projint = 1 / 3) {
  if (n < 1) stop("sample size must be at least 1.")
  if (gtheta < 0 || gtheta > 1) stop("G(theta) must lie in [0, 1].")
  projvar <- projint - gtheta^2
  mean <- n * (n + 1) / 2 * gtheta
  v <- n * (n + 1)^2 * projvar
  list(mean = mean, variance = v, se = if (v > 0) sqrt(v) else NA_real_,
       projvar = projvar, n = n,
       method = "smoothed Wilcoxon signed rank mean and variance (Sec. 5.3.1)")
}

# CANONICAL TEST
# r <- Swilmom(n = 10); stopifnot(abs(r$variance - 10 * 121 / 12) < 1e-9)

#' @rdname Swilmom
#' @keywords internal
#' @export
morie_fauzi_wilcoxon_moments <- Swilmom
