# SPDX-License-Identifier: AGPL-3.0-or-later

#' The Poisson pmf sums to one
#'
#' Sums the Poisson pmf over k = 0 .. kmax - 1.
#'
#' @param a expected count, >= 0.
#' @param kmax truncation point, >= 1.
#' @return list(total, error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.11).
#' @examples
#' PoisSum1(3)$error
#' @export
PoisSum1 <- function(a, kmax = 200) {
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a) || a < 0) {
    stop("a must be a single value >= 0.", call. = FALSE)
  }
  if (length(kmax) != 1L || is.na(kmax) || kmax < 1 ||
        kmax != as.integer(kmax)) {
    stop("kmax must be a single integer >= 1.", call. = FALSE)
  }
  total <- 0
  for (k in 0:(as.integer(kmax) - 1L)) total <- total + PoisPmf(k, a)$probability
  list(total = total, error = abs(total - 1))
}
