# SPDX-License-Identifier: AGPL-3.0-or-later

#' Geometric extrapolation of the expected KDFE (Theorem 2.1)
#'
#' Theorem 2.1: for any `a > 0`, `a != 1`,
#' \deqn{\[J_h(x)\]^{t_1}\[J_{ah}(x)\]^{t_2} = F_X(x) + O(h^4),\quad t_1 = \frac{a^2}{a^2-1},\ t_2 = -\frac{1}{a^2-1},}{\[J_h(x)\]^t1 \[J_ah(x)\]^t2 = F(x) + O(h^4), t1 = a^2/(a^2-1), t2 = -1/(a^2-1),}
#' with `J_h = E\[Fhat_h\]`. The exponents solve `t1 + t2 = 1` (keep `log F`)
#' and `t1 + a^2 t2 = 0` (kill the `h^2` term).
#'
#' Contrast Chapter 1, where the same device forced the bandwidth ratio to 4:
#' there the expansion ran in `sqrt(h)`, so the second condition was
#' `t1 + 2 t2 = 0`, which pins the ratio. Here it runs in `h^2` and `a` stays
#' FREE -- a genuine second smoothing parameter which, per Remark 2.2, need
#' not depend on `n`. Large `a` returns the plain KDFE; `a` near 0 is unwise,
#' since `a` divides the argument of `W`.
#'
#' The exponents come back so the caller can see they sum to 1 -- the
#' invariant that makes the result a distribution function value.
#'
#' @param jh `J_h(x) = E\[Fhat_h(x)\]`.
#' @param jah `J_ah(x)`, the same at bandwidth `a h`.
#' @param a Second smoothing parameter; `a > 0`, `a != 1`.
#' @return Named list with ``estimate``, ``t1``, ``t2``, ``a``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 2.1.
#' @examples
#' Kdfgeoext(jh = 0.5, jah = 0.5, a = 2)
#' @export
Kdfgeoext <- function(jh, jah, a) {
  if (a <= 0) stop("a must be positive.")
  if (a == 1) stop("a = 1 is excluded: the exponents divide by a^2 - 1.")
  if (jh <= 0 || jah <= 0) stop("J_h and J_ah must be positive; the identity takes logs.")
  t1 <- a * a / (a * a - 1); t2 <- -1 / (a * a - 1)
  list(estimate = jh^t1 * jah^t2, t1 = t1, t2 = t2, a = a,
       method = "geometric extrapolation of E[hat F_h] (Theorem 2.1)")
}

# CANONICAL TEST
# r <- Kdfgeoext(jh = 0.5, jah = 0.5, a = 2)
# stopifnot(abs(r$estimate - 0.5) < 1e-15, abs(r$t1 + r$t2 - 1) < 1e-15)

#' @rdname Kdfgeoext
#' @keywords internal
#' @export
morie_fauzi_thm2_1_expected_kdfe <- Kdfgeoext
