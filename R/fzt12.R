# SPDX-License-Identifier: AGPL-3.0-or-later

#' Geometric extrapolation of the raw gamma-kernel mean (Theorem 1.2)
#'
#' Theorem 1.2, Eq. (1.13): \eqn{J_h^2(x)\[J_{4h}(x)\]^{-1} = f_X(x) + O(h)}{J_h^2(x)\[J_4h(x)\]^-1 = f(x) + O(h)},
#' with \eqn{J_h = E\[A_h\]}{J_h = E\[A_h\]}.
#'
#' The exponents are not a guess. Writing
#' `log J_h = log f + a sqrt(h)/f + (b - a^2/(2f)) h/f`, the pair
#' `(t1, t2)` must satisfy `t1 + t2 = 1` (keep `log f`) and
#' `t1 + 2 t2 = 0` (kill `sqrt(h)`); the unique solution is `(2, -1)`.
#' The `O(sqrt(h))` bias of Theorem 1.1 is cancelled by a weighted
#' GEOMETRIC mean of two smoothings, not an arithmetic one, because the
#' expansion is multiplicative in `f`.
#'
#' Supply `a`, `b` and `f` to also get the explicit `O(h)` remainder
#' `-2(b - a^2/(2f)) h`; without them `remainder` is `NA`.
#'
#' @param jh `J_h(x) = E\[A_h(x)\]`.
#' @param j4h `J_4h(x) = E\[A_4h(x)\]`, the same object at bandwidth `4h`.
#' @param h Bandwidth; needed only for the explicit remainder.
#' @param a,b,f The coefficients (1.16), (1.17) and `f(x)`.
#' @return Named list with ``estimate``, ``remainder``, ``t1``, ``t2``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 1.2, Eq. (1.13).
#' @examples
#' Gkgeoext(jh = 0.4, j4h = 0.5)
#' @export
Gkgeoext <- function(jh, j4h, h = NULL, a = NULL, b = NULL, f = NULL) {
  if (j4h == 0) stop("J_4h(x) must be non-zero -- (1.13) divides by it.")
  est <- jh * jh / j4h
  rem <- NA_real_
  if (!is.null(h) && !is.null(a) && !is.null(b) && !is.null(f)) {
    if (f == 0) stop("f(x) must be non-zero in the (1.13) remainder.")
    rem <- -2 * (b - a^2 / (2 * f)) * h
  }
  list(estimate = est, remainder = rem, t1 = 2, t2 = -1,
       method = "geometric extrapolation of E[A_h] (Theorem 1.2)")
}

# CANONICAL TEST
# r <- Gkgeoext(jh = 0.4, j4h = 0.5); stopifnot(abs(r$estimate - 0.32) < 1e-15)

#' @rdname Gkgeoext
#' @keywords internal
#' @export
morie_fauzi_thm1_2_var_mgkde <- Gkgeoext
