# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial mean
#'
#' np, cross-checked against sum k P(k) over the full pmf.
#'
#' @param n trials, >= 0.
#' @param p success probability, in \[0, 1\].
#' @return list(mean, series_mean).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.61).
#' @examples
#' BinomMean(10, 0.3)$mean
#' @export
BinomMean <- function(n, p) {
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
  if (abs(sum(pmf) - 1) > 1e-9) stop("binomial pmf does not sum to 1.", call. = FALSE)
  value <- n * p
  series <- sum(ks * pmf)
  if (abs(series - value) > 1e-9 * max(1, value)) {
    stop("series mean disagrees with np.", call. = FALSE)
  }
  list(mean = value, series_mean = series)
}
