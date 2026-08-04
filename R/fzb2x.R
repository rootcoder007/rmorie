# SPDX-License-Identifier: AGPL-3.0-or-later

#' The b_2 bias coefficient of the kernel distribution function estimator
#'
#' Eq. (2.7): \deqn{b_2(x) = \frac{f_X'(x)}{2}\int w^2 K(w)\,dw.}{b2(x) = (f'(x)/2) int w^2 K(w) dw.}
#'
#' The leading bias coefficient of the KDFE:
#' `Bias[Fhat_h(x)] = h^2 b2(x) + o(h^2)`.
#'
#' Note which derivative appears. A kernel DENSITY estimator's leading bias
#' carries `f''`; the distribution-function estimator carries `f'`, one order
#' lower, because it smooths with the INTEGRATED kernel `W`. That single fact
#' is why the book's bandwidth rate is `n^(-1/3)` and not `n^(-1/5)`.
#'
#' `mu2` defaults to 1, the Gaussian value; pass `0.2` for Epanechnikov. It is
#' an explicit argument, never estimated from data -- the kernel is a
#' modelling choice, not a random quantity.
#'
#'
#' Naming note. The backlog assigns this row the public name
#' `fauzi_b2_coefficient`, but `morie_fauzi_b2_coefficient` is already exported
#' from `R/fauzi_native.R` for the Chapter 4 coefficient `b_2(t)` of Eq.
#' (4.15) -- a different quantity that merely shares the book's symbol.
#' Defining it again here would silently rebind that export, since
#' `fauzi_native.R` collates first. The legacy spelling kept here is therefore
#' `morie_fauzi_b2_coefficient_kdfe`.
#' @param fp `f'(x)`.
#' @param mu2 `int w^2 K(w) dw`; 1 for the Gaussian kernel.
#' @return Named list with ``estimate``, ``mu2``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (2.7).
#' @examples
#' Kdfb2(fp = 0.4)
#' @export
Kdfb2 <- function(fp, mu2 = 1) {
  list(estimate = fp / 2 * mu2, mu2 = mu2,
       method = "b_2 bias coefficient of the KDFE (Eq. 2.7)")
}

# CANONICAL TEST
# stopifnot(Kdfb2(fp = 0.4)$estimate == 0.2)

#' @rdname Kdfb2
#' @keywords internal
#' @export
morie_fauzi_b2_coefficient_kdfe <- Kdfb2
