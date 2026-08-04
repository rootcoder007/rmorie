# SPDX-License-Identifier: AGPL-3.0-or-later

#' Centred fair-coin binomial
#'
#' P(n + x Heads in 2n fair flips) = C(2n, n+x)/2^(2n).
#'
#' @param x deviation from n Heads; zero outside abs(x) <= n.
#' @param n half the number of flips, >= 0.
#' @return list(x, n, probability).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (5.3).
#' @examples
#' BinomCtr(1, 10)$probability
#' @export
BinomCtr <- function(x, n) {
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  if (length(x) != 1L || is.na(x) || x != as.integer(x)) {
    stop("x must be a single integer.", call. = FALSE)
  }
  n <- as.integer(n)
  x <- as.integer(x)
  value <- if (abs(x) > n) 0 else BinomPmf(n + x, 2L * n, 0.5)$probability
  list(x = x, n = n, probability = value)
}
