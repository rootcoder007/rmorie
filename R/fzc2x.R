# SPDX-License-Identifier: AGPL-3.0-or-later

#' The c_2 bias coefficient of the boundary-free KDE (Theorem 5.5)
#'
#' From Theorem 5.5:
#' \deqn{c_2(x) = g^{(3)}(g^{-1}(x))f_X(x) + 3g''(g^{-1}(x))g'(g^{-1}(x))f_X'(x) + [g'(g^{-1}(x))]^3 f_X''(x).}{c2(x) = g3(g^-1(x)) f(x) + 3 g''(g^-1(x)) g'(g^-1(x)) f'(x) + [g'(g^-1(x))]^3 f''(x).}
#'
#' The coefficients 1, 3, 1 and the derivative orders are the Faa di Bruno
#' pattern for the second derivative of a composition -- which is what this is,
#' since the estimator lives on the transformed scale and the bias is read back
#' on the original one.
#'
#' It enters the bias of the boundary-free DENSITY estimator divided by `g'`:
#' `Bias[ftilde(x)] = h^2 c2(x) mu2(K) / (2 g'(g^-1(x))) + o(h^2)`. That
#' division is the Jacobian which the DISTRIBUTION estimator of (5.5) does not
#' need -- the clearest statement in the book of why the transformation trick
#' is cheaper for distribution functions.
#'
#' With `g` the identity this collapses to `f''(x)`, the classical
#' kernel-density bias coefficient.
#'
#' @param dg,d2g,d3g `g'`, `g''` and `g3` evaluated at `g^-1(x)`.
#' @param density `f_X(x)`.
#' @param fp,fpp `f'(x)` and `f''(x)`.
#' @return Named list with ``estimate``, ``scaled``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.5.
#' @examples
#' Bfc2(dg = 1, d2g = 0, d3g = 0, density = 0.3, fp = 0.2, fpp = -0.1)
#' @export
Bfc2 <- function(dg, d2g, d3g, density, fp, fpp) {
  if (dg == 0) stop("g'(g^-1(x)) must be non-zero; the bias divides by it.")
  val <- d3g * density + 3 * d2g * dg * fp + dg^3 * fpp
  list(estimate = val, scaled = val / dg,
       method = "c_2 bias coefficient of the boundary-free KDE (Theorem 5.5)")
}

# CANONICAL TEST
# r <- Bfc2(dg = 1, d2g = 0, d3g = 0, density = 0.3, fp = 0.2, fpp = -0.1)
# stopifnot(abs(r$estimate + 0.1) < 1e-15)

#' @rdname Bfc2
#' @keywords internal
#' @export
morie_fauzi_c2_coefficient <- Bfc2
