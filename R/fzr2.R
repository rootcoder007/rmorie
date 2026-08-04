# SPDX-License-Identifier: AGPL-3.0-or-later

#' The kernel constant r_2 of the bias-reduced KDFE variance
#'
#' Eq. (2.10):
#' \deqn{r_2 = \int y K(y)[W(y/a) + a^{-1}W(y)K(y/a)]\,dy.}{r2 = int y [K(y) W(y/a) + (1/a) W(y) K(y/a)] dy.}
#'
#' Read the bracket carefully: the two terms differ in WHICH factor is
#' rescaled by `a`. It is the cross term from expanding the variance of the
#' linear combination `a^2/(a^2-1) Fhat_h - 1/(a^2-1) Fhat_ah` in Theorem 2.3,
#' so it is not symmetric in the two bandwidths and must not be "simplified"
#' into `2 r1`.
#'
#' At `a = 1` the two terms coincide and `r2 = 2 r1`, but `a = 1` is excluded
#' by Theorem 2.1 anyway (`a^2 - 1` is a denominator).
#'
#' Integrated on a fixed trapezoid grid for every kernel including the
#' Gaussian: the book states no closed form, and inventing one would be worse
#' than integrating.
#'
#' @param a The second smoothing parameter of (2.5); `a > 0`, `a != 1`.
#' @param kernel `"gaussian"` or a function `K(y)`.
#' @param lo,hi Quadrature limits.
#' @param ngrid Number of nodes; fixed, never adapted.
#' @return Named list with ``estimate``, ``a``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (2.10).
#' @examples
#' Kdfr2(a = 2)
#' @export
Kdfr2 <- function(a, kernel = "gaussian", lo = -8, hi = 8, ngrid = 4001L) {
  if (a <= 0) stop("a must be positive.")
  if (a == 1) stop("a = 1 is excluded: (2.5) divides by a^2 - 1.")
  y <- seq(lo, hi, length.out = ngrid)
  if (identical(kernel, "gaussian")) {
    kfun <- stats::dnorm; wfun <- stats::pnorm
  } else if (is.function(kernel)) {
    kfun <- function(t) vapply(t, function(u) as.numeric(kernel(u)), numeric(1))
    base <- kfun(y)
    cum <- c(0, cumsum(diff(y) * (base[-length(base)] + base[-1]) / 2))
    wfun <- function(t) stats::approx(y, cum, xout = t, rule = 2)$y
  } else {
    stop("kernel must be \"gaussian\" or a function K(y).")
  }
  term <- kfun(y) * wfun(y / a) + (1 / a) * wfun(y) * kfun(y / a)
  trap <- function(v, g) sum(diff(g) * (v[-length(v)] + v[-1]) / 2)
  list(estimate = trap(y * term, y), a = a,
       method = "r_2 cross-kernel constant (Eq. 2.10)")
}

# CANONICAL TEST
# r1 <- 1 / (2 * sqrt(pi))
# stopifnot(abs(Kdfr2(a = 1.0000001)$estimate - 2 * r1) < 1e-5)

#' @rdname Kdfr2
#' @keywords internal
#' @export
morie_fauzi_r2_integral <- Kdfr2
