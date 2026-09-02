# SPDX-License-Identifier: AGPL-3.0-or-later
#' RKHS estimation in the frequentist framework.
#'
#' Formula: min over (eta_0, beta) of {(1/n) sum_i L(y_i, eta_0 + k_i'beta)
#' + (lambda/2) beta'K beta} (eq. 8.3), obtained by substituting the representer
#' form (8.2) into (8.1). beta'K beta is the empirical RKHS norm and lambda
#' controls the trade-off between goodness of fit and complexity; with the
#' squared-error loss the stationarity conditions are linear and are solved
#' directly.
#'
#' @param K Kernel matrix.
#' @param y Response vector.
#' @param lam Complexity weight.
#' @return List with estimate, eta0, beta, fitted, objective, penalty, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (8.3) p.254. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm128(K = 5L, y = 5L)
Msm128 <- function(K, y, lam = 1) {
  f <- .gprkhsfitsq(K, y, lam = lam)
  list(estimate = f$eta0, eta0 = f$eta0, beta = f$beta, fitted = f$fitted,
       objective = f$objective, penalty = f$penalty,
       method = "RKHS frequentist fit (MVSML 2022 eq. 8.3)")
}
