# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial probability mass function
#'
#' Exact binomial coefficients up to n = 1000, log-gamma above it.
#'
#' @param k successes, >= 0.
#' @param n trials, >= 0.
#' @param p success probability, in [0, 1].
#' @return list(k, n, p, probability).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (4.6), (4.60).
#' @examples
#' BinomPmf(2, 4, 0.5)$probability
#' @export
BinomPmf <- function(k, n, p) {
  if (length(k) != 1L || is.na(k) || k < 0 || k != as.integer(k)) {
    stop("k must be a single integer >= 0.", call. = FALSE)
  }
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  p <- as.numeric(p)
  if (length(p) != 1L || is.na(p) || p < 0 || p > 1) {
    stop("p must be a single value in [0, 1].", call. = FALSE)
  }
  k <- as.integer(k)
  n <- as.integer(n)
  value <- if (k > n) {
    0
  } else if (p == 0) {
    as.numeric(k == 0L)
  } else if (p == 1) {
    as.numeric(k == n)
  } else if (n <= 1000L) {
    choose(n, k) * p^k * (1 - p)^(n - k)
  } else {
    exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) +
          k * log(p) + (n - k) * log1p(-p))
  }
  list(k = k, n = n, p = p, probability = value)
}
