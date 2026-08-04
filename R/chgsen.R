# SPDX-License-Identifier: AGPL-3.0-or-later
#' Change-of-variance function and sensitivity (Hampel et al. 1986)
#'
#' For a location M-estimator with score psi at a symmetric model F the
#' asymptotic variance is V = E[psi^2] / (E[psi-prime])^2 and the
#' change-of-variance function is
#' CVF(x) = V * (1 + psi(x)^2/E[psi^2] - 2 psi-prime(x)/E[psi-prime]),
#' with sensitivity kappa* = sup CVF(x)/V.  Source consulted: Hampel,
#' Ronchetti, Rousseeuw and Stahel (1986), Robust Statistics, section 2.5.
#'
#' @param psi numeric vector, the score function on a grid.
#' @param x optional numeric grid; defaults to 0, 1, ..., n-1.
#' @param dpsi optional numeric derivative of psi; central differences if NULL.
#' @param w optional probability weights for the expectations.
#' @return list: estimate, kappa_star, V, cvfmax, cvfmin, cvf, vrobust, n, method.
#' @keywords internal
#' @examples
#' chgsen(c(-1, 0, 1), x = c(-1, 0, 1), dpsi = c(1, 1, 1))
#' @export
chgsen <- function(psi, x = NULL, dpsi = NULL, w = NULL) {
  psi <- as.numeric(psi)
  n <- length(psi)
  xg <- if (is.null(x)) seq_len(n) - 1 else as.numeric(x)
  if (is.null(dpsi)) {
    dp <- numeric(n)
    for (i in seq_len(n)) {
      if (i == 1) dp[i] <- (psi[2] - psi[1]) / (xg[2] - xg[1])
      else if (i == n) dp[i] <- (psi[n] - psi[n - 1]) / (xg[n] - xg[n - 1])
      else dp[i] <- (psi[i + 1] - psi[i - 1]) / (xg[i + 1] - xg[i - 1])
    }
  } else dp <- as.numeric(dpsi)
  ww <- if (is.null(w)) rep(1 / n, n) else as.numeric(w) / sum(as.numeric(w))
  a <- sum(ww * psi * psi)
  b <- sum(ww * dp)
  v <- if (b != 0) a / (b * b) else NA_real_
  cvf <- v * (1 + psi^2 / a - 2 * dp / b)
  cvfmax <- max(cvf)
  kappa <- if (!is.na(v) && v != 0) cvfmax / v else NA_real_
  list(estimate = kappa, kappa_star = kappa, V = as.numeric(v),
       cvfmax = as.numeric(cvfmax), cvfmin = as.numeric(min(cvf)), cvf = cvf,
       vrobust = is.finite(kappa), n = as.integer(n),
       method = "Change-of-variance sensitivity (Hampel et al. 1986)")
}

# CANONICAL TEST
# r <- chgsen(c(-1, 0, 1), x = c(-1, 0, 1), dpsi = c(1, 1, 1))
# stopifnot(abs(r$V - 2 / 3) < 1e-12)

#' @rdname chgsen
#' @keywords internal
#' @export
morie_change_of_variance <- chgsen
