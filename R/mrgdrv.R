# SPDX-License-Identifier: AGPL-3.0-or-later
#' Azuma-Hoeffding concentration bound for martingales
#'
#' Formula: P(M_n - M_0 >= t) <= exp(-t^2 / (2 sum_i c_i^2))
#'
#' @param c Bounded-difference constants c_i, one per step.
#' @param t Deviation whose probability is bounded.

#' @param c See Usage.
#' @param t See Usage.
#' @return List with ``bound`` (one-sided), ``bound_two_sided``, ``sum_c2``, ``t``, ``n``.
#' @references Azuma (1967), Weighted sums of certain dependent random variables, Tohoku Mathematical Journal 19:357-367; Hoeffding (1963), JASA 58:13-30. Neither is held locally; the inequality is stated in this exact form in every standard reference consulted.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Azuma(V, V)
Azuma <- function(c, t) {
  cc <- .t1_vec(c)
  t <- as.numeric(t)
  if (any(cc < 0)) stop("bounded-difference constants must be non-negative")
  s <- sum(cc^2)
  if (s <= 0) stop("sum of squared differences must be positive")
  b <- exp(-t^2 / (2 * s))
  .t1_result(bound = b, bound_two_sided = min(1, 2 * b), sum_c2 = s,
             t = t, n = length(cc), method = "Azuma-Hoeffding bound")
}
