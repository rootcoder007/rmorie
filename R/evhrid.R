# SPDX-License-Identifier: AGPL-3.0-or-later

#' Husler-Reiss bivariate dependence
#'
#' Formula: F(x,y) = exp(-x Phi(lam + log(y/x)/(2 lam))
#'                       - y Phi(lam + log(x/y)/(2 lam)))
#'
#' Written on the standard exponential scale, where each margin is
#' exp(-x).  As lam -> 0 the exponent tends to min(x, y), perfect
#' dependence; as lam -> infinity it tends to x + y, independence.
#'
#' @param x First coordinate, strictly positive.
#' @param y Second coordinate, strictly positive.
#' @param lam Dependence parameter, strictly positive.
#' @return List with \code{F}, \code{estimate}, \code{V},
#'   \code{A_half}, \code{chi}, \code{n}, \code{method}.
#' @references Husler & Reiss (1989), Statist. Probab. Letters
#'   7(4):283-286.
#' @export
#' @examples
#' Evhrid(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L)
Evhrid <- function(x, y, lam) {
  xs <- .s03vec(x)
  ys <- .s03vec(y)
  lam <- as.numeric(lam)
  if (!length(xs) || !length(ys)) stop("empty input: x and y are required")
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  if (!(lam > 0)) stop("lam must be strictly positive")
  if (any(xs <= 0) || any(ys <= 0)) stop("x and y must be strictly positive")
  V <- numeric(length(xs))
  FF <- numeric(length(xs))
  for (i in seq_along(xs)) {
    a <- xs[i]
    b <- ys[i]
    V[i] <- a * .s03pnorm(lam + log(b / a) / (2 * lam)) +
      b * .s03pnorm(lam + log(a / b) / (2 * lam))
    FF[i] <- exp(-V[i])
  }
  .t1_result(F = FF, estimate = FF[1], V = V, A_half = .s03pnorm(lam),
             chi = 2 - 2 * .s03pnorm(lam), n = length(xs),
             method = "Husler-Reiss bivariate extreme-value dependence")
}
