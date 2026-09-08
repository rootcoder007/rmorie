# SPDX-License-Identifier: AGPL-3.0-or-later

#' Sample correlation coefficient
#'
#' Uses the 1/n covariance and the 1/n standard deviations, so the
#' divisors cancel and r matches the book's deviation form.
#'
#' @param x,y equal-length data vectors, n >= 2, neither constant.
#' @return list(r, cov).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (6.12), (6.55).
#' @examples
#' SampR(c(2, 3, 3, 5, 7), c(1, 1, 3, 4, 6))$r
#' @export
SampR <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y) || length(x) < 2L || any(is.na(x)) || any(is.na(y))) {
    stop("x and y must be equal-length vectors, n >= 2.", call. = FALSE)
  }
  cov <- mean((x - mean(x)) * (y - mean(y)))
  sx <- sqrt(mean((x - mean(x))^2))
  sy <- sqrt(mean((y - mean(y))^2))
  if (sx == 0 || sy == 0) stop("degenerate data: zero variance.", call. = FALSE)
  list(r = cov / (sx * sy), cov = cov)
}
