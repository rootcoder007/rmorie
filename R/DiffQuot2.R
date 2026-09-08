# SPDX-License-Identifier: AGPL-3.0-or-later

#' Difference quotient of x squared
#'
#' ((x+d)^2 - x^2)/d, cross-checked against the closed form 2x + d, and
#' the limit 2x.
#'
#' @param x evaluation point.
#' @param delta step, nonzero.
#' @return list(quotient, derivative_limit).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.31).
#' @examples
#' DiffQuot2(3, 0.001)$quotient
#' @export
DiffQuot2 <- function(x, delta) {
  x <- as.numeric(x)
  delta <- as.numeric(delta)
  if (length(x) != 1L || is.na(x)) stop("x must be a single value.", call. = FALSE)
  if (length(delta) != 1L || is.na(delta) || delta == 0) {
    stop("delta must be a single nonzero value.", call. = FALSE)
  }
  quotient <- ((x + delta)^2 - x^2) / delta
  explicit <- 2 * x + delta
  if (abs(quotient - explicit) > 1e-9 * max(1, abs(explicit))) {
    stop("quotient != 2x + delta.", call. = FALSE)
  }
  list(quotient = quotient, derivative_limit = 2 * x)
}
