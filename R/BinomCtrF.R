# SPDX-License-Identifier: AGPL-3.0-or-later

#' Factorial form of the centred binomial
#'
#' (2n)!/((n+x)!(n-x)! 2^(2n)), cross-checked against the
#' C(2n, n+x)/2^(2n) form.
#'
#' @param x deviation from n Heads; zero outside abs(x) <= n.
#' @param n half the number of flips, >= 0.
#' @return list(probability).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (5.5).
#' @examples
#' BinomCtrF(1, 10)$probability
#' @export
BinomCtrF <- function(x, n) {
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  if (length(x) != 1L || is.na(x) || x != as.integer(x)) {
    stop("x must be a single integer.", call. = FALSE)
  }
  n <- as.integer(n)
  x <- as.integer(x)
  value <- if (abs(x) > n) {
    0
  } else {
    choose(2 * n, n + x) / 4^n
  }
  check <- BinomCtr(x, n)$probability
  if (abs(value - check) > 1e-12 * max(1, check)) {
    stop("factorial form disagrees with C(2n, n+x)/2^2n.", call. = FALSE)
  }
  list(probability = value)
}
