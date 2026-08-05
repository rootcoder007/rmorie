# SPDX-License-Identifier: AGPL-3.0-or-later

#' Least-squares line
#'
#' A = (<xy> - <x><y>)/(<x^2> - <x>^2) and B = <y> - A<x>; the ratio
#' form of B is computed independently and the two must agree.  The
#' defaults are the book's five-point worked data set.
#'
#' @param x,y equal-length data vectors, n >= 2.
#' @return list(A, B, S).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (6.42)-(6.49), (6.82).
#' @examples
#' LsqFit()$A
#' @export
LsqFit <- function(x = NULL, y = NULL) {
  if (is.null(x)) x <- c(2, 3, 3, 5, 7)
  if (is.null(y)) y <- c(1, 1, 3, 4, 6)
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y) || length(x) < 2L || any(is.na(x)) || any(is.na(y))) {
    stop("x and y must be equal-length, n >= 2.", call. = FALSE)
  }
  mx <- mean(x); my <- mean(y); mxy <- mean(x * y); mx2 <- mean(x * x)
  denom <- mx2 - mx * mx
  if (denom == 0) stop("all x identical: slope undefined.", call. = FALSE)
  a_slope <- (mxy - mx * my) / denom
  b_first <- (my * mx2 - mx * mxy) / denom
  b_second <- my - a_slope * mx
  if (abs(b_first - b_second) > 1e-9 * max(1, abs(b_first))) {
    stop("the two intercept forms disagree.", call. = FALSE)
  }
  resid <- y - (a_slope * x + b_second)
  if (abs(sum(resid)) > 1e-9 * max(1, sum(abs(resid)))) {
    stop("residuals do not sum to zero.", call. = FALSE)
  }
  list(A = a_slope, B = b_second, S = sum(resid^2))
}
