# SPDX-License-Identifier: AGPL-3.0-or-later
#' Periodogram of a process observed on a rectangular lattice
#'
#' eq (4.57): \eqn{I(\omega_1,\omega_2) = \{(2\pi)^2 rc\}^{-1} |\sum_u
#' \sum_v Z(u,v) \exp\{-i(\omega_1 u + \omega_2 v)\}|^2} at the Fourier
#' frequencies, the multiples of \eqn{2\pi/r} and \eqn{2\pi/c}.
#'
#' The section's central claim is eq (4.59): away from the origin the
#' periodogram *is* the Fourier transform of the sample covariance function.
#' With `check_identity` the right-hand side is computed independently and
#' the largest discrepancy returned; it should sit at machine precision.
#' That check pins the \eqn{(2\pi)^2} normalisation, which the placeholder
#' this replaces had dropped in favour of \eqn{1/n}.
#'
#' @param z_lattice The r by c lattice of observations.
#' @param coords Accepted and ignored; the estimator is defined on the
#'   row-column lattice, not on arbitrary coordinates.
#' @param omit_zero_frequency Remove the mean first (default `TRUE`). At a
#'   non-zero Fourier frequency this changes nothing (p. 191); at the origin
#'   it removes the squared mean, where eq (4.59) does not apply.
#' @param check_identity Verify eq (4.59) numerically (default `TRUE`).
#' @return A list with `periodogram`, `omega1`, `omega2`, `j`, `k`,
#'   `covariance`, `lags_j`, `lags_k`, `mean_invariant`, `nonzero_mask`,
#'   `r`, `c`, and when checked `identity_max_abs_diff`, `identity_holds`
#'   and `periodogram_from_covariance`.
#' @references Schabenberger Ch 4, Sec 4.7.1, eqs (4.56)-(4.59), pp. 190-192
#' @export
spperiod <- function(z_lattice, coords = NULL, omit_zero_frequency = TRUE,
                     check_identity = TRUE) {
  p <- .schab_periodogram(z_lattice, omit_zero_frequency = omit_zero_frequency)
  sc <- .schab_sample_cov2d(z_lattice)
  out <- list(periodogram = p$periodogram, omega1 = p$omega1,
              omega2 = p$omega2, j = p$j, k = p$k, covariance = sc$cov,
              lags_j = sc$lags_j, lags_k = sc$lags_k,
              mean_invariant = p$mean_invariant,
              nonzero_mask = p$nonzero_mask, r = p$r, c = p$c)
  if (check_identity) {
    q <- .schab_periodogram_from_cov(z_lattice)
    d <- max(abs(p$periodogram[p$nonzero_mask] -
                 q$periodogram[p$nonzero_mask]))
    out$identity_max_abs_diff <- d
    out$identity_holds <- d < 1e-8
    out$periodogram_from_covariance <- q$periodogram
    if (!out$identity_holds) {
      out$warning <- paste0(
        "the periodogram does not match the Fourier transform of the ",
        "sample covariance function; eq (4.59) should hold to machine ",
        "precision away from the origin")
    }
  }
  out
}

