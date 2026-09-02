# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lasso-penalized multinomial log-likelihood.
#'
#' Formula: l_p(beta; y) = l(beta; y) - lambda sum_c sum_j |beta_cj|
#' (eq. 7.10): the same block updating as eq. (7.9) but with the quadratic
#' penalty replaced by an L1 one.
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
#'   Springer, eq. (7.10) p.227. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm115(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = 5L, beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5)
Msm115 <- function(X, y, beta0, beta, lam = 1, baseline_last = TRUE) {
  f <- .gppenmnloglik(X, y, beta0, beta, lam, penalty = "lasso",
                      baseline_last = baseline_last)
  list(estimate = f$penalized_loglik, loglik = f$loglik, penalty = f$penalty,
       method = "lasso-penalized multinomial log-likelihood (MVSML 2022 eq. 7.10)")
}
