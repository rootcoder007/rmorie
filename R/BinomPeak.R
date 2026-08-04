# SPDX-License-Identifier: AGPL-3.0-or-later

#' Peak height of the binomial distribution
#'
#' The binomial pmf evaluated at k = round(pn).
#'
#' @param n trials, >= 0.
#' @param p success probability, in [0, 1].
#' @return list(k, PB).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.96).
#' @examples
#' BinomPeak(100, 0.3)$PB
#' @export
BinomPeak <- function(n, p) {
  n <- as.integer(n)
  p <- as.numeric(p)
  if (is.na(n) || n < 0L || is.na(p) || p < 0 || p > 1) {
    stop("need n >= 0 integer and p in [0, 1].", call. = FALSE)
  }
  k <- as.integer(round(p * n))
  list(k = k, PB = BinomPmf(k, n, p)$probability)
}
