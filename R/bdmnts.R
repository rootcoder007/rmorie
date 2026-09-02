# SPDX-License-Identifier: AGPL-3.0-or-later
#' Monotone instrumental variable bounds.
#'
#' LB(t|v) = max_{u <= v} LB(t|u), UB(t|v) = min_{u >= v} UB(t|u), then
#' average over P(V = v).
#'
#' @param lower,upper Per-value bounds in increasing order of v.
#' @param prob P(V = v), same order; normalised internally.
#'
#' @return List with lower, upper, width, lowerv, upperv, prob, k.
#' @references Manski and Pepper (2000), Econometrica 68(4), 997-1010.
#'   Standard published form; the article could not be obtained (JSTOR
#'   access stub, NBER t0224 zero-page PDF) and was not read.
#' @export
#' @examples
#' Mivbound(lower = 5L, upper = 5L, prob = 0.5)
Mivbound <- function(lower, upper, prob) {
  L <- .t1_vec(lower); U <- .t1_vec(upper); p <- .t1_vec(prob)
  k <- length(L)
  if (length(U) != k || length(p) != k)
    stop("lower, upper and prob must have the same length")
  if (k == 0) stop("need at least one instrument value")
  if (any(p < 0)) stop("probabilities must be non-negative")
  tot <- sum(p)
  if (tot <= 0) stop("probabilities must not all be zero")
  p <- p / tot
  Lv <- cummax(L)
  Uv <- rev(cummin(rev(U)))
  .t1_result(lower = sum(p * Lv), upper = sum(p * Uv),
             width = sum(p * Uv) - sum(p * Lv), lowerv = Lv, upperv = Uv,
             prob = p, k = k,
             method = "Monotone instrumental variable bounds (Manski-Pepper 2000)")
}
