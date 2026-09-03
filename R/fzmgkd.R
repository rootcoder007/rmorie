# SPDX-License-Identifier: AGPL-3.0-or-later

#' Modified gamma kernel density estimator (Eq. 1.14)
#'
#' Eq. (1.14): \eqn{\tilde f_X(x) = \[A_h(x)\]^2 \[A_{4h}(x)\]^{-1}}{ftilde(x) = A_h(x)^2
#' / A_4h(x)},
#' with `A_h` the raw gamma-kernel function (1.9) -- a sample mean of
#' Gamma(`h^(-1/2)`, `x sqrt(h) + h`) densities.
#'
#' The exponents 2 and -1 are the `(t1, t2)` of Theorem 1.2, and the factor 4
#' on the bandwidth is fixed by that theorem too; neither is tunable, which is
#' why neither is a parameter. The estimator is non-negative by construction,
#' unlike the order-4 kernel alternative.
#'
#' Chapter 1 fixes the bandwidth ratio at 4 because its expansion is in
#' `sqrt(h)`; the free ratio `a` belongs to the Chapter 2 DISTRIBUTION-function
#' construction (2.5), whose expansion is in `h^2`.
#'
#' @param x Sample on `[0, infinity)`.
#' @param grid Points at which to evaluate the density.
#' @param h Bandwidth, `h > 0`.
#' @return Named list with ``estimate``, ``ah``, ``a4h``, ``grid``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eqs. (1.9) and (1.14).
#' @examples
#' Mgkde(c(0.5, 1, 1.5, 2, 2.5), grid = 1, h = 0.2)
#' @export
Mgkde <- function(x, grid, h) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 1L) stop("need at least one observation.")
  g <- as.numeric(grid)
  ah <- .morie_fauzi_agamma(x, g, h)
  a4h <- .morie_fauzi_agamma(x, g, 4 * h)
  if (any(a4h <= 0)) stop("A_4h vanished on the grid; (1.14) divides by it.")
  list(estimate = ah^2 / a4h, ah = ah, a4h = a4h, grid = g, h = h, n = n,
       method = "modified gamma kernel density estimator (Eq. 1.14)")
}

# CANONICAL TEST
# r <- Mgkde(c(0.5, 1, 1.5, 2, 2.5), grid = 1, h = 0.2)
# stopifnot(abs(r$estimate - r$ah^2 / r$a4h) < 1e-15)

#' @rdname Mgkde
#' @keywords internal
#' @export
morie_fauzi_modified_gamma_kde <- Mgkde
