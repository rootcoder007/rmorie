# SPDX-License-Identifier: AGPL-3.0-or-later

#' Difference quotient of x to the n
#'
#' ((x+d)^n - x^n)/d beside the derivative n x^(n-1).
#'
#' @param x evaluation point.
#' @param n power, >= 0.
#' @param delta step, nonzero.
#' @return list(quotient, derivative, abs_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.33).
#' @examples
#' DiffQuotN(3, 5, 1e-6)$abs_error
#' @export
DiffQuotN <- function(x, n, delta) {
  x <- as.numeric(x)
  delta <- as.numeric(delta)
  if (length(x) != 1L || is.na(x)) stop("x must be a single value.", call. = FALSE)
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  if (length(delta) != 1L || is.na(delta) || delta == 0) {
    stop("delta must be a single nonzero value.", call. = FALSE)
  }
  n <- as.integer(n)
  quotient <- ((x + delta)^n - x^n) / delta
  derivative <- if (n >= 1L) n * x^(n - 1) else 0
  list(quotient = quotient, derivative = derivative,
       abs_error = abs(quotient - derivative))
}
