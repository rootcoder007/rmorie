# SPDX-License-Identifier: AGPL-3.0-or-later
#' Integral of a basis expansion
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer:
#' for x(t) = sum_k c_k phi_k(t) the integral is linear in the coefficients,
#' integral x(t) dt = sum_k c_k integral phi_k(t) dt, so integrating a fitted
#' curve reduces to integrating each basis function once.
#'
#' The basis integrals are taken by the composite trapezoid rule over the
#' WHOLE grid, end intervals included.
#'
#' @param coef K coefficients.
#' @param basis n-by-K matrix of basis functions on the grid.
#' @param t the grid; defaults to equally spaced on [0, 1].
#' @return list: estimate, basis_integrals, n, nbasis, method.
#' @keywords internal
#' @examples
#' Intf(c(3, 4), cbind(1, seq(0, 1, length.out = 5)))$estimate
#' @export
Intf <- function(coef, basis, t = NULL) {
  cc <- .s03vec(coef)
  B <- .s03mat(basis)
  n <- nrow(B)
  K <- ncol(B)
  if (n < 2L) stop("integrate_function: need at least two grid points")
  if (length(cc) != K) stop("integrate_function: coef must have one entry per basis column")
  tt <- if (is.null(t)) .fdgrid(n) else .s03vec(t)
  if (length(tt) != n) stop("integrate_function: t must match the number of basis rows")
  ints <- numeric(K)
  for (j in seq_len(K)) ints[j] <- .fdtrapz(tt, B[, j])
  total <- 0
  for (j in seq_len(K)) total <- total + cc[j] * ints[j]
  list(estimate = total, basis_integrals = ints, n = n, nbasis = K,
       method = "Ramsay-Silverman (2005) linearity of the integral over a basis expansion, trapezoid over the whole grid")
}
