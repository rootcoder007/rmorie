# SPDX-License-Identifier: AGPL-3.0-or-later

#' Check conditions D1-D5 of the boundary-free kernel estimators
#'
#' The five conditions of Sec. 5.2, checked rather than asserted:
#'
#' * D1: `K` non-negative, continuous, symmetric at 0.
#' * D2: `int v^2 K(v) dv` finite and `int K(v) dv = 1`.
#' * D3: `h > 0`, `h -> 0`, `n h -> Inf`; checked as `0 < h < 1`, `n h > 1`.
#' * D4: `g` an INCREASING bijection from the real line onto the support
#'   `Omega`; checked by monotonicity on a fixed grid.
#' * D5: `f_X` and `g` twice differentiable -- a smoothness claim, reported as
#'   `NA` unless the caller asserts it.
#'
#' The book says something worth repeating: it is SUFFICIENT for `g` to be
#' bijective, and the increasing property in D4 is imposed only to make the
#' proofs simpler. A decreasing bijection is not wrong, merely outside what is
#' proved -- which is why a decreasing `g` returns `d4 = FALSE` with the
#' direction in `monotone`, rather than an error.
#'
#' D2 is checked as a pair: a kernel can integrate to 1 and still have an
#' infinite second moment (Cauchy), so mass and `mu2` are reported separately.
#'
#' @param kernel `K(v)`; defaults to the Gaussian density.
#' @param g The transformation, for D4.
#' @param h Bandwidth, for D3.
#' @param n Sample size, for D3.
#' @param smooth The caller's assertion of D5.
#' @param tol Tolerance for the D1/D2 checks.
#' @param lo,hi,ngrid Fixed quadrature window and node count.
#' @return Named list with ``d1``, ``d2``, ``d3``, ``d4``, ``d5``, ``mass``, ``mu2``, ``monotone``, ``method``.
#' @references Fauzi and Maesono (2023), conditions D1-D5 of Sec. 5.2.
#' @examples
#' Bfassum(h = 0.1, n = 100, g = exp)
#' @export
Bfassum <- function(kernel = NULL, g = NULL, h = NULL, n = NULL, smooth = NA, tol = 1e-6, lo = -8, hi = 8, ngrid = 4001L) {
  kfun <- if (is.null(kernel)) stats::dnorm else {
    if (!is.function(kernel)) stop("kernel must be NULL or a function K(v).")
    kernel
  }
  v <- seq(lo, hi, length.out = ngrid)
  kv <- vapply(v, function(t) as.numeric(kfun(t)), numeric(1))
  trap <- function(y, gg) sum(diff(gg) * (y[-length(y)] + y[-1]) / 2)
  mass <- trap(kv, v)
  sym <- max(abs(kv - rev(kv)))
  mu2 <- trap(v^2 * kv, v)
  d1 <- all(kv >= 0) && sym < tol
  d2 <- is.finite(mu2) && abs(mass - 1) < tol
  d3 <- if (is.null(h) || is.null(n)) NA else (h > 0 && h < 1 && n * h > 1)
  if (is.null(g)) {
    d4 <- NA
    monotone <- "unknown"
  } else {
    gv <- vapply(v, function(t) as.numeric(g(t)), numeric(1))
    dv <- diff(gv)
    monotone <- if (all(dv > 0)) "increasing" else if (all(dv < 0)) "decreasing" else "neither"
    d4 <- identical(monotone, "increasing")
  }
  list(d1 = d1, d2 = d2, d3 = d3, d4 = d4, d5 = as.logical(smooth),
       mass = mass, mu2 = mu2, monotone = monotone,
       method = "conditions D1-D5 of the boundary-free kernel estimators")
}

# CANONICAL TEST
# r <- Bfassum(h = 0.1, n = 100, g = exp); stopifnot(r$d1, r$d2, r$d3, r$d4)

#' @rdname Bfassum
#' @keywords internal
#' @export
morie_fauzi_conditions_d1_d5 <- Bfassum
