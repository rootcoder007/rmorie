# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of the bias-reduced KDFE (Theorem 2.3)
#'
#' Theorem 2.3:
#' \deqn{\mathrm{Var}[\tilde F_X(x)] = \frac{F(1-F)}{n} - \frac{h}{n}\Big[\frac{2(a^4+1)}{(a^2-1)^2}r_1 + r_2\Big]f_X(x) + o(h/n),}{Var[Ftilde(x)] = F(1-F)/n - (h/n)[2(a^4+1)/(a^2-1)^2 r1 + r2] f(x) + o(h/n),}
#' with `r1` from (2.9) and `r2` from (2.10).
#'
#' The ORDER does not change from the plain KDFE -- it cannot, since `Ftilde`
#' linearises to a fixed linear combination of two KDFEs -- but the CONSTANT
#' does, and the book's claim is that it is smaller. That is the content: bias
#' improved from `O(h^2)` to `O(h^4)` at no cost in variance order, and a gain
#' in its constant.
#'
#' The bracket blows up as `a -> 1`: `(a^2-1)^2` is in the denominator and the
#' estimator is undefined there. Small `a` is fine; `a` near 1 is not, and the
#' function refuses it rather than returning a huge number that looks like a
#' result.
#'
#' @param n Sample size.
#' @param h Bandwidth.
#' @param a Second smoothing parameter; `a > 0`, `a != 1`.
#' @param fx `F_X(x)`.
#' @param density `f_X(x)`.
#' @param r1,r2 Kernel constants; default to the Gaussian `r1` and the `r2`
#'   evaluated at this `a`.
#' @return Named list with ``variance``, ``se``, ``edfvar``, ``gain``, ``r1``, ``r2``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 2.3, Eqs. (2.9)-(2.10).
#' @examples
#' Gekdfvar(n = 100, h = 0.2, a = 2, fx = 0.5, density = 0.4)
#' @export
Gekdfvar <- function(n, h, a, fx, density, r1 = NULL, r2 = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  if (a <= 0) stop("a must be positive.")
  if (abs(a - 1) < 1e-6) stop("a too close to 1: (a^2 - 1)^2 divides the variance.")
  if (is.null(r1)) r1 <- Kdfr1()$estimate
  if (is.null(r2)) r2 <- Kdfr2(a = a)$estimate
  bracket <- 2 * (a^4 + 1) / (a * a - 1)^2 * r1 + r2
  edfvar <- fx * (1 - fx) / n
  v <- edfvar - h / n * bracket * density
  list(variance = v, se = if (v > 0) sqrt(v) else NA_real_, edfvar = edfvar,
       gain = edfvar - v, r1 = r1, r2 = r2,
       method = "variance of the bias-reduced KDFE (Theorem 2.3)")
}

# CANONICAL TEST
# r <- Gekdfvar(n = 100, h = 0.2, a = 2, fx = 0.5, density = 0.4)
# stopifnot(r$variance < r$edfvar)

#' @rdname Gekdfvar
#' @keywords internal
#' @export
morie_fauzi_thm2_3_var_brdkdfe <- Gekdfvar
