# SPDX-License-Identifier: AGPL-3.0-or-later

#' Gaussian limit of the Poisson distribution
#'
#' e^(-(k-a)^2/(2a))/sqrt(2 pi a), beside the exact Poisson pmf at
#' round(k).
#'
#' @param k event count; may be non-integral for the continuous form.
#' @param a expected count, > 0.
#' @return list(PG, exact).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (5.23).
#' @examples
#' PoisGauss(12, 10)$PG
#' @export
PoisGauss <- function(k, a) {
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a) || a <= 0) {
    stop("a must be a single value > 0.", call. = FALSE)
  }
  k <- as.numeric(k)
  if (length(k) != 1L || is.na(k)) stop("k must be a single value.", call. = FALSE)
  value <- exp(-(k - a)^2 / (2 * a)) / sqrt(2 * pi * a)
  exact <- if (k >= 0) PoisPmf(as.integer(round(k)), a)$probability else 0
  list(PG = value, exact = exact)
}
