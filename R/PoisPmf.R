# SPDX-License-Identifier: AGPL-3.0-or-later

#' Poisson probability mass function
#'
#' Evaluated in log space so large k does not overflow.
#'
#' @param k event count, >= 0.
#' @param a expected count, >= 0.
#' @return list(k, a, probability).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.40).
#' @examples
#' PoisPmf(3, 2)$probability
#' @export
PoisPmf <- function(k, a) {
  if (length(k) != 1L || is.na(k) || k < 0 || k != as.integer(k)) {
    stop("k must be a single integer >= 0.", call. = FALSE)
  }
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a) || a < 0) {
    stop("a must be a single value >= 0.", call. = FALSE)
  }
  k <- as.integer(k)
  value <- if (a > 0) exp(k * log(a) - a - lgamma(k + 1)) else as.numeric(k == 0L)
  list(k = k, a = a, probability = value)
}
