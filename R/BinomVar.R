# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial variance
#'
#' The moment identity E(k^2) - (np)^2, cross-checked against npq.
#'
#' @param n trials, >= 0.
#' @param p success probability, in \[0, 1\].
#' @return list(variance).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.67).
#' @examples
#' BinomVar(10, 0.3)$variance
#' @export
BinomVar <- function(n, p) {
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  p <- as.numeric(p)
  if (length(p) != 1L || is.na(p) || p < 0 || p > 1) {
    stop("p must be a single value in [0, 1].", call. = FALSE)
  }
  n <- as.integer(n)
  value <- (p^2 * n * (n - 1) + p * n) - (n * p)^2
  direct <- n * p * (1 - p)
  if (abs(value - direct) > 1e-9 * max(1, direct)) {
    stop("moment identity disagrees with npq.", call. = FALSE)
  }
  list(variance = value)
}
