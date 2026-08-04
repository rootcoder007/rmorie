# SPDX-License-Identifier: AGPL-3.0-or-later

#' The b_4 bias coefficient of the kernel distribution function estimator
#'
#' Eq. (2.8): \deqn{b_4(x) = \frac{f_X^{(3)}(x)}{24}\int w^4 K(w)\,dw.}{b4(x) = (f3(x)/24) int w^4 K(w) dw.}
#'
#' The second bias coefficient in
#' `J_h(x) = F(x)(1 + h^2 b2/F + h^4 b4/F) + o(h^4)`. It only matters once
#' `b2` has been eliminated -- which is exactly what the geometric
#' extrapolation of Theorem 2.1 does, leaving
#' `h^4 a^2 (b2^2 - 2 b4 F) / (2 F)` as the whole bias.
#'
#' So `b4` is not a refinement of the standard KDFE's error; it IS the error
#' of the bias-reduced one. Assumptions B2 (finite `mu4(K)`) and B4
#' (`f^(4)` exists) are needed for precisely this term.
#'
#' `mu4` defaults to 3, the Gaussian value.
#'
#' @param fppp The third derivative `f^(3)(x)`.
#' @param mu4 `int w^4 K(w) dw`; 3 for the Gaussian kernel.
#' @return Named list with ``estimate``, ``mu4``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (2.8).
#' @examples
#' Kdfb4(fppp = 24)
#' @export
Kdfb4 <- function(fppp, mu4 = 3) {
  list(estimate = fppp / 24 * mu4, mu4 = mu4,
       method = "b_4 bias coefficient of the KDFE (Eq. 2.8)")
}

# CANONICAL TEST
# stopifnot(abs(Kdfb4(fppp = 24)$estimate - 3) < 1e-15)

#' @rdname Kdfb4
#' @keywords internal
#' @export
morie_fauzi_b4_coefficient <- Kdfb4
