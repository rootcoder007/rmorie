# SPDX-License-Identifier: AGPL-3.0-or-later

#' Poisson-Stirling pmf in centred variables
#'
#' The Stirling Poisson pmf evaluated at k = round(a + x).
#'
#' @param x_dev deviation from the mean; k = round(a + x_dev) must be >= 1.
#' @param a expected count, > 0.
#' @return list(k, approx, exact).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (5.17).
#' @examples
#' PoisStirC(2, 8)$approx
#' @export
PoisStirC <- function(x_dev, a) {
  k <- as.integer(round(as.numeric(a) + as.numeric(x_dev)))
  if (is.na(k) || k < 1L) stop("a + x must round to k >= 1.", call. = FALSE)
  r <- PoisStirl(k, a)
  list(k = k, approx = r$approx, exact = r$exact)
}
