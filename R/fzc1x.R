# SPDX-License-Identifier: AGPL-3.0-or-later

#' The c_1 bias coefficient of the boundary-free KDFE (Eq. 5.8)
#'
#' Eq. (5.8):
#' \deqn{c_1(x) = g''(g^{-1}(x))f_X(x) + \[g'(g^{-1}(x))\]^2 f_X'(x),}{c1(x) =
#' g''(g^-1(x)) f(x) + \[g'(g^-1(x))\]^2 f'(x),}
#' the coefficient in `Bias\[Ftilde(x)\] = (h^2/2) c1(x) mu2(K) + o(h^2)` of
#' Theorem 5.2.
#'
#' Compare the naive KDFE, whose bias coefficient is just `f'(x) mu2 / 2` --
#' i.e. (5.8) with `g` the identity, where `g'' = 0` and `g' = 1`. The
#' transformation adds one term and rescales the other; that is the entire cost
#' of removing the boundary bias.
#'
#' This is the same expression as `b1(t)` in (4.14) of Chapter 4: the survival
#' and distribution estimators are the same construction applied to `1-F` and
#' `F`, so they share a bias coefficient -- and Theorem 4.1 duly carries a
#' minus sign in front of it where Theorem 5.2 does not.
#'
#' @param dg,d2g `g'(g^-1(x))` and `g''(g^-1(x))`.
#' @param density `f_X(x)`.
#' @param fp `f_X'(x)`.
#' @return Named list with ``estimate``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (5.8); the same expression as (4.14).
#' @examples
#' Bfc1(dg = 1, d2g = 0, density = 0.3, fp = 0.2)
#' @export
Bfc1 <- function(dg, d2g, density, fp) {
  list(estimate = d2g * density + dg^2 * fp,
       method = "c_1 bias coefficient of the boundary-free KDFE (Eq. 5.8)")
}

# CANONICAL TEST
# stopifnot(abs(Bfc1(dg = 1, d2g = 0, density = 0.3, fp = 0.2)$estimate - 0.2) < 1e-15)

#' @rdname Bfc1
#' @keywords internal
#' @export
morie_fauzi_c1_coefficient <- Bfc1
