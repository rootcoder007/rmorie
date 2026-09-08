# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bivariate logistic max-stable distribution
#'
#' Formula: F(x,y) = exp(-((x^(-1/alpha) + y^(-1/alpha))^alpha))
#'
#' Coles (2001) eq. (8.10), p. 146, on unit Frechet margins.  As
#' alpha -> 1 the exponent becomes 1/x + 1/y and the margins are
#' independent; as alpha -> 0 it becomes max(1/x, 1/y) and they are
#' perfectly dependent.  The Pickands function of this family is
#' A(t) = (t^(1/alpha) + (1-t)^(1/alpha))^alpha and the coefficient of
#' upper tail dependence is 2 - 2^alpha.
#'
#' @param x First coordinate, strictly positive (unit Frechet scale).
#' @param y Second coordinate, strictly positive.
#' @param alpha Dependence parameter in (0, 1].
#' @return List with \code{F}, \code{estimate}, \code{V},
#'   \code{A_half}, \code{chi}, \code{n}, \code{method}.
#' @references Tawn (1988), Biometrika 75(3):397-415; Coles (2001), An
#'   Introduction to Statistical Modeling of Extreme Values, Springer,
#'   eq. (8.10) p. 146.
#' @export
#' @examples
#' Evmsexp(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), alpha = 0.5)
Evmsexp <- function(x, y, alpha) {
  xs <- .s03vec(x)
  ys <- .s03vec(y)
  alpha <- as.numeric(alpha)
  if (!length(xs) || !length(ys)) stop("empty input: x and y are required")
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  if (!(alpha > 0 && alpha <= 1)) stop("alpha must lie in (0, 1]")
  if (any(xs <= 0) || any(ys <= 0))
    stop("x and y must be strictly positive on the Frechet scale")
  V <- numeric(length(xs))
  FF <- numeric(length(xs))
  for (i in seq_along(xs)) {
    # in logs: a small alpha sends x^(-1/alpha) below the smallest
    # double and the exponent collapses to zero, so the perfect
    # dependence limit V -> max(1/x, 1/y) would be lost entirely.
    la <- -log(xs[i]) / alpha
    lb <- -log(ys[i]) / alpha
    m <- if (la > lb) la else lb
    V[i] <- exp(alpha * (m + log1p(exp(-abs(la - lb)))))
    FF[i] <- exp(-V[i])
  }
  a_half <- (0.5^(1 / alpha) + 0.5^(1 / alpha))^alpha
  .t1_result(F = FF, estimate = FF[1], V = V, A_half = a_half,
             chi = 2 - 2^alpha, n = length(xs),
             method = "bivariate logistic max-stable distribution")
}
