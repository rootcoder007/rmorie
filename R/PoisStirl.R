# SPDX-License-Identifier: AGPL-3.0-or-later

#' Poisson pmf under Stirling's factorial approximation
#'
#' a^k e^-a / k! with k! replaced by k^k e^-k sqrt(2 pi k), beside the
#' exact pmf.
#'
#' @param k event count, >= 1.
#' @param a expected count, > 0.
#' @return list(approx, exact, rel_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (5.16).
#' @examples
#' PoisStirl(10, 8)$rel_error
#' @export
PoisStirl <- function(k, a) {
  if (length(k) != 1L || is.na(k) || k < 1 || k != as.integer(k)) {
    stop("k must be a single integer >= 1.", call. = FALSE)
  }
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a) || a <= 0) {
    stop("a must be a single value > 0.", call. = FALSE)
  }
  k <- as.integer(k)
  approx <- exp(k * log(a) - a - (k * log(k) - k + 0.5 * log(2 * pi * k)))
  exact <- exp(k * log(a) - a - lgamma(k + 1))
  list(approx = approx, exact = exact,
       rel_error = abs(approx - exact) / max(exact, 1e-300))
}
