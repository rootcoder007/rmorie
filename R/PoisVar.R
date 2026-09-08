# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of the Poisson distribution by its series
#'
#' The series E(k^2) minus the squared series mean, truncated 15
#' standard deviations above a.
#'
#' @param a expected count, >= 0.
#' @return list(variance).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.94).
#' @examples
#' PoisVar(3)$variance
#' @export
PoisVar <- function(a) {
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a) || a < 0) {
    stop("a must be a single value >= 0.", call. = FALSE)
  }
  ks <- 0:(max(50, floor(a + 15 * sqrt(a + 1))) - 1)
  pmf <- vapply(ks, function(k) PoisPmf(k, a)$probability, numeric(1))
  mu <- sum(ks * pmf)
  list(variance = sum(ks^2 * pmf) - mu^2)
}
