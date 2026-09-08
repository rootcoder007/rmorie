# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial success probability equalising P(0) and P(1)
#'
#' p = 1/(n+1), reported with the two pmf values it equalises.
#'
#' @param n trials, >= 1.
#' @return list(p, P0, P1).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.9).
#' @examples
#' BinomPeq(5)$p
#' @export
BinomPeq <- function(n) {
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("n must be a single integer >= 1.", call. = FALSE)
  }
  n <- as.integer(n)
  value <- 1 / (n + 1)
  p0 <- BinomPmf(0, n, value)$probability
  p1 <- BinomPmf(1, n, value)$probability
  if (abs(p0 - p1) > 1e-12) stop("P(0) != P(1) at p = 1/(n+1).", call. = FALSE)
  list(p = value, P0 = p0, P1 = p1)
}
