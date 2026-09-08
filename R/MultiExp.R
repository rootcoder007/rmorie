# SPDX-License-Identifier: AGPL-3.0-or-later

#' Multinomial theorem expansion
#'
#' (x1 + ... + xk)^N summed over every composition n1 + ... + nk = N of
#' C(N; n1,...,nk) x1^n1 ... xk^nk, eq (1.38), with the multinomial
#' coefficient of eq (1.37).  The total is cross-checked against the
#' direct power.
#'
#' @param xs the k values x1..xk.
#' @param N the power, N >= 0.
#' @return list(k, power, n_terms, expansion, direct_power, max_coefficient).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.37)-(1.38).
#' @examples
#' MultiExp(c(1, 1, 1), 4)$n_terms
#' @export
MultiExp <- function(xs, N) {
  xs <- as.numeric(xs)
  if (length(xs) == 0L || any(is.na(xs))) {
    stop("xs must be a non-empty numeric vector.", call. = FALSE)
  }
  N <- as.integer(N)
  if (is.na(N) || N < 0L) {
    stop("N must be a non-negative integer.", call. = FALSE)
  }
  k <- length(xs)
  comps <- function(total, parts) {
    if (parts == 1L) return(list(total))
    out <- list()
    for (first in 0:total) {
      for (rest in comps(total - first, parts - 1L)) {
        out[[length(out) + 1L]] <- c(first, rest)
      }
    }
    out
  }
  all_comps <- comps(N, k)
  total <- 0
  max_coef <- 0
  for (comp in all_comps) {
    coef <- morie_multinomial_coef(comp)
    total <- total + coef * prod(xs^comp)
    if (coef > max_coef) max_coef <- coef
  }
  direct <- sum(xs)^N
  if (abs(total - direct) > 1e-9 * max(1, abs(direct))) {
    stop("multinomial expansion does not match the direct power.", call. = FALSE)
  }
  list(k = as.numeric(k), power = as.numeric(N),
       n_terms = as.numeric(length(all_comps)), expansion = as.numeric(total),
       direct_power = as.numeric(direct), max_coefficient = as.numeric(max_coef))
}
