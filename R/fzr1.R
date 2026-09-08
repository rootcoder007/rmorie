# SPDX-License-Identifier: AGPL-3.0-or-later

#' The kernel constant r_1 of the KDFE variance
#'
#' Eq. (2.9): \deqn{r_1 = \int y K(y) W(y)\,dy,}{r1 = int y K(y) W(y) dy,}
#' with `W(y) = int_{-inf}^{y} K(v) dv`.
#'
#' This is the constant that makes the book worth writing. The KDFE variance
#' (2.4) is `F(1-F)/n - 2 h r1 f(x) / n`: the bandwidth enters with a NEGATIVE
#' sign, so smoothing REDUCES variance -- the opposite of a density estimator,
#' where it enters at `1/(nh)`. Sec. 2.1 concludes that any kernel with
#' `r1 > 0` beats the empirical df for every `F_X`, and proves `r1 >= 0` for
#' symmetric kernels by splitting the integral at 0.
#'
#' For the Gaussian kernel the value is exactly `1/(2 sqrt(pi))`, returned in
#' closed form. Any other kernel is integrated on a fixed trapezoid grid --
#' fixed node count, no adaptive refinement, so two calls agree bitwise.
#'
#' @param kernel `"gaussian"` for the closed form, or a function `K(y)`.
#' @param lo,hi Quadrature limits, used only for a callable kernel.
#' @param ngrid Number of nodes; fixed, never adapted.
#' @return Named list with ``estimate``, ``kernel``, ``method``.
#' @references Fauzi and Maesono (2023), Eqs. (2.4) and (2.9).
#' @examples
#' Kdfr1()
#' @export
Kdfr1 <- function(kernel = "gaussian", lo = -8, hi = 8, ngrid = 4001L) {
  if (identical(kernel, "gaussian")) {
    return(list(estimate = 1 / (2 * sqrt(pi)), kernel = "gaussian",
                method = "r_1 = int y K(y) W(y) dy, closed form (Eq. 2.9)"))
  }
  if (!is.function(kernel)) stop("kernel must be \"gaussian\" or a function K(y).")
  y <- seq(lo, hi, length.out = ngrid)
  kv <- vapply(y, function(t) as.numeric(kernel(t)), numeric(1))
  trap <- function(v, g) sum(diff(g) * (v[-length(v)] + v[-1]) / 2)
  wv <- c(0, cumsum(diff(y) * (kv[-length(kv)] + kv[-1]) / 2))
  list(estimate = trap(y * kv * wv, y), kernel = "callable",
       method = "r_1 = int y K(y) W(y) dy, trapezoid (Eq. 2.9)")
}

# CANONICAL TEST
# stopifnot(abs(Kdfr1()$estimate - 0.2820947917738781) < 1e-15)

#' @rdname Kdfr1
#' @keywords internal
#' @export
morie_fauzi_r1_integral <- Kdfr1
