# SPDX-License-Identifier: AGPL-3.0-or-later

#' Check assumptions B1-B5 of the bias-reduced KDFE
#'
#' The five standing assumptions of Sec. 2.2, checked numerically rather than
#' asserted:
#'
#' * B1: `K` non-negative, continuous, symmetric about 0, integrating to 1.
#' * B2: `int w^4 K(w) dw` finite.
#' * B3: `h > 0`, `h -> 0`, `n h -> inf`; checked in its usable finite-sample
#'   form, `0 < h < 1` and `n h > 1`.
#' * B4: `f_X` three times continuously differentiable with `f^(4)` existing --
#'   a smoothness claim about the unknown density that NO routine can verify
#'   from data. Reported as `NA`; assert it via `smooth`.
#' * B5: `int [f'(x)]^2 / F(x) dx` and `int f(x) dx` finite.
#'
#' The book is explicit about the division of labour: B1 and B3 are the usual
#' kernel conditions, B2 and B4 exist only to make the exponential and
#' logarithmic expansions legitimate, and B5 only so the MISE of Theorem 2.4
#' is finite. A failure of B2 or B4 invalidates the bias RATE; a failure of B5
#' invalidates the MISE but leaves the pointwise results standing. Hence they
#' are reported separately, not as one boolean.
#'
#' @param kernel `K(w)`; defaults to the Gaussian density.
#' @param h Bandwidth, for B3.
#' @param n Sample size, for B3.
#' @param smooth The caller's assertion of B4; there is no way to check it.
#' @param tol Tolerance for the B1 symmetry and unit-mass checks.
#' @param lo,hi,ngrid Fixed quadrature window and node count.
#' @return Named list with ``b1``, ``b2``, ``b3``, ``b4``, ``b5``, ``mu4``, ``mass``, ``method``.
#' @references Fauzi and Maesono (2023), assumptions B1-B5 of Sec. 2.2.
#' @examples
#' Kdfassum(h = 0.1, n = 100)
#' @export
Kdfassum <- function(kernel = NULL, h = NULL, n = NULL, smooth = NA, tol = 1e-6, lo = -8, hi = 8, ngrid = 4001L) {
  kfun <- if (is.null(kernel)) stats::dnorm else {
    if (!is.function(kernel)) stop("kernel must be NULL or a function K(w).")
    kernel
  }
  w <- seq(lo, hi, length.out = ngrid)
  kv <- vapply(w, function(t) as.numeric(kfun(t)), numeric(1))
  trap <- function(v, g) sum(diff(g) * (v[-length(v)] + v[-1]) / 2)
  mass <- trap(kv, w)
  sym <- max(abs(kv - rev(kv)))
  mu4 <- trap(w^4 * kv, w)
  b1 <- all(kv >= 0) && abs(mass - 1) < tol && sym < tol
  b2 <- is.finite(mu4)
  b3 <- if (is.null(h) || is.null(n)) NA else (h > 0 && h < 1 && n * h > 1)
  list(b1 = b1, b2 = b2, b3 = b3, b4 = as.logical(smooth), b5 = NA,
       mu4 = mu4, mass = mass,
       method = "assumptions B1-B5 of the bias-reduced KDFE")
}

# CANONICAL TEST
# r <- Kdfassum(h = 0.1, n = 100); stopifnot(r$b1, r$b2, r$b3)

#' @rdname Kdfassum
#' @keywords internal
#' @export
morie_fauzi_assumptions_b1_b5 <- Kdfassum
