# SPDX-License-Identifier: AGPL-3.0-or-later
#' RKHS optimization problem.
#'
#' Formula: min over f in H of {(1/n) sum_i L(y_i, f(x_i)) + lambda ||f||_H^2}
#' (eq. 8.1), the penalized empirical risk in a reproducing kernel Hilbert
#' space, where L is minus the conditional log-likelihood for the response type
#' and ||f||_H^2 measures model complexity. The objective is evaluated at a
#' given (eta_0, beta).
#'
#' @param K Kernel matrix.
#' @param y Response vector.
#' @param beta Representer coefficients.
#' @param eta0 Intercept on the latent scale.
#' @param lam Complexity weight.
#' @param loss One of "squared", "logistic" or "hinge".
#' @return List with estimate, objective, empirical_risk, rkhs_norm2, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (8.1) p.254. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm123(K = 5L, y = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5)
Msm123 <- function(K, y, beta, eta0 = 0, lam = 1, loss = "squared") {
  f <- .gprkhspredict(K, beta, eta0)
  ys <- .gpflat(y)
  n <- length(ys)
  emp <- switch(loss,
    squared = sum((ys - f)^2) / n,
    logistic = sum(log(1 + exp(-(2 * ys - 1) * f))) / n,
    hinge = sum(pmax(0, 1 - (2 * ys - 1) * f)) / n,
    stop(sprintf("unknown loss: %s", loss)))
  norm <- .gprkhsnorm(beta, K)
  obj <- emp + as.numeric(lam) * norm
  list(estimate = obj, objective = obj, empirical_risk = emp,
       rkhs_norm2 = norm, method = "RKHS penalized risk (MVSML 2022 eq. 8.1)")
}
