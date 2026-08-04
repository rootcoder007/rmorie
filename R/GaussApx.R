# SPDX-License-Identifier: AGPL-3.0-or-later

#' Gaussian approximation to the centred fair-coin binomial
#'
#' e^(-x^2/n)/sqrt(pi n) beside the exact centred binomial.
#'
#' @param x deviation from n Heads.
#' @param n half the number of flips, >= 1.
#' @return list(approx, exact, rel_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (5.4), (5.13).
#' @examples
#' GaussApx(2, 50)$rel_error
#' @export
GaussApx <- function(x, n) {
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("n must be a single integer >= 1.", call. = FALSE)
  }
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x)) stop("x must be a single value.", call. = FALSE)
  n <- as.integer(n)
  approx <- exp(-x * x / n) / sqrt(pi * n)
  exact <- BinomCtr(as.integer(round(x)), n)$probability
  list(approx = approx, exact = exact,
       rel_error = abs(approx - exact) / max(exact, 1e-300))
}
