# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial second moment
#'
#' p^2 n(n-1) + pn, cross-checked against sum k^2 P(k).
#'
#' @param n trials, >= 0.
#' @param p success probability, in [0, 1].
#' @return list(second_moment).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.66).
#' @examples
#' BinomMom2(10, 0.3)$second_moment
#' @export
BinomMom2 <- function(n, p) {
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  p <- as.numeric(p)
  if (length(p) != 1L || is.na(p) || p < 0 || p > 1) {
    stop("p must be a single value in [0, 1].", call. = FALSE)
  }
  n <- as.integer(n)
  ks <- 0:n
  pmf <- vapply(ks, function(k) BinomPmf(k, n, p)$probability, numeric(1))
  value <- p^2 * n * (n - 1) + p * n
  series <- sum(ks^2 * pmf)
  if (abs(series - value) > 1e-9 * max(1, value)) {
    stop("series second moment disagrees.", call. = FALSE)
  }
  list(second_moment = value)
}
