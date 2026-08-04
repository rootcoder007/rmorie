# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial expansion of a shifted power
#'
#' sum_k C(n,k) x^(n-k) d^k, beside the closed value (x+d)^n.
#'
#' @param x base.
#' @param n power, >= 0.
#' @param delta shift.
#' @return list(terms, sum, exact, abs_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.35).
#' @examples
#' BinomExp(2, 5, 0.1)$abs_error
#' @export
BinomExp <- function(x, n, delta) {
  x <- as.numeric(x)
  delta <- as.numeric(delta)
  if (length(x) != 1L || is.na(x)) stop("x must be a single value.", call. = FALSE)
  if (length(delta) != 1L || is.na(delta)) {
    stop("delta must be a single value.", call. = FALSE)
  }
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  n <- as.integer(n)
  ks <- 0:n
  terms <- choose(n, ks) * x^(n - ks) * delta^ks
  total <- sum(terms)
  exact <- (x + delta)^n
  list(terms = terms, sum = total, exact = exact,
       abs_error = abs(total - exact))
}
