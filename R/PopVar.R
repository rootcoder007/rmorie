# SPDX-License-Identifier: AGPL-3.0-or-later

#' Population variance of a data set
#'
#' s-tilde^2 = (1/n) sum (xi - xbar)^2 (eqs 3.37, 3.60), reported beside
#' the identity (1/n) sum (xi - xbar)^2 = mean(x^2) - xbar^2 (eq 3.66)
#' evaluated by its own route.
#'
#' @param x numeric data, non-empty.
#' @return list(variance, n, lhs, rhs, identity_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.37), (3.60), (3.66).
#' @examples
#' PopVar(c(2, 4, 4, 4, 5, 5, 7, 9))$variance
#' @export
PopVar <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0L || any(is.na(x))) {
    stop("x must be a non-empty numeric vector.", call. = FALSE)
  }
  xbar <- mean(x)
  lhs <- mean((x - xbar)^2)
  rhs <- mean(x^2) - xbar^2
  if (abs(lhs - rhs) > 1e-9 * max(1, abs(lhs))) {
    stop("definition and computational forms disagree.", call. = FALSE)
  }
  list(variance = lhs, n = length(x), lhs = lhs, rhs = rhs,
       identity_error = abs(lhs - rhs))
}
