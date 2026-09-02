# SPDX-License-Identifier: AGPL-3.0-or-later
#' Penalized multinomial log-likelihood (ridge).
#'
#' Formula: l_p(beta; y) = l(beta; y) - lambda sum_c beta_c'beta_c (eq. 7.7),
#' the quadratic-regularized multinomial likelihood, which removes the need for
#' the identifiability constraint on the slopes. Intercepts are never penalized.
#'
#' @param X Design matrix.
#' @param y Class labels, zero-based.
#' @param beta0 Intercepts, one per non-baseline category.
#' @param beta Slope matrix, one row per non-baseline category.
#' @param lam Penalty weight.
#' @param baseline_last Fix the last category at zero for identifiability.
#' @return List with estimate, loglik, penalty, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (7.7) p.226. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm109(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = 5L, beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5)
Msm109 <- function(X, y, beta0, beta, lam = 1, baseline_last = TRUE) {
  f <- .gppenmnloglik(X, y, beta0, beta, lam, penalty = "ridge",
                      baseline_last = baseline_last)
  list(estimate = f$penalized_loglik, loglik = f$loglik, penalty = f$penalty,
       method = "ridge-penalized multinomial log-likelihood (MVSML 2022 eq. 7.7)")
}
