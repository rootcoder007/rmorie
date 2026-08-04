# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial pmf for a b-sided die
#'
#' Success probability p = 1/b.
#'
#' @param k successes, >= 0.
#' @param n rolls, >= 0.
#' @param b number of faces, >= 1.
#' @return list(k, n, p, probability).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.32).
#' @examples
#' BinomDie(2, 10, 6)$probability
#' @export
BinomDie <- function(k, n, b) {
  b <- as.numeric(b)
  if (length(b) != 1L || is.na(b) || b < 1) stop("b must be >= 1.", call. = FALSE)
  r <- BinomPmf(k, n, 1 / b)
  list(k = r$k, n = r$n, p = 1 / b, probability = r$probability)
}
