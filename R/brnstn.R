# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bernstein's concentration inequality.
#'
#' Formula: P(S_n >= n t) <= exp(-n t^2 / (2 sigma^2 + 2 M t / 3))
#'
#' @param sigma2 Bound on the per-summand variance.
#' @param M Almost-sure bound on |X_i - E X_i|.
#' @param n Number of summands.
#' @param t Deviation per summand.

#' @return List with ``bound``, ``bound_two_sided``, ``hoeffding``, ``ratio``, ``exponent``.
#' @references Bernstein (1924). The original is not held locally and is in Russian; the inequality is stated in this exact form in every standard concentration-inequality reference consulted.
#' @export
Bernstein <- function(sigma2, M, n, t) {
  s2 <- as.numeric(sigma2); M <- as.numeric(M)
  n <- as.integer(n); t <- as.numeric(t)
  if (s2 < 0 || M < 0 || n < 1 || t < 0)
    stop("need sigma2 >= 0, M >= 0, n >= 1, t >= 0")
  den <- 2 * s2 + 2 * M * t / 3
  if (den <= 0) stop("degenerate bound: sigma2 and M are both zero")
  ex <- -n * t^2 / den; b <- exp(ex)
  hoef <- if (M > 0) exp(-n * t^2 / (2 * M^2)) else 0
  .t1_result(bound = b, bound_two_sided = min(1, 2 * b), hoeffding = hoef,
             ratio = if (hoef > 0) b / hoef else Inf, exponent = ex,
             method = "Bernstein inequality")
}
