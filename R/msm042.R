# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normal linear regression under the Bayesian paradigm.
#'
#' Formula: Y = beta_0 + sum_j X_j beta_j + eps (eq. 6.1) with the improper
#' reference prior f(beta, sigma2) proportional to sigma^-2 (eq. 6.2). When X
#' has full column rank the posterior is proper:
#' sigma2 | y ~ IG((n-p-1)/2, (n-p-1) s2 / 2) and
#' beta | sigma2, y ~ N(beta-hat, sigma2 (X'X)^-1), with beta-hat the OLS
#' estimator and s2 = y'(I - H)y/(n - p - 1).
#'
#' @param X Design matrix.
#' @param y Response vector.
#' @param add_intercept Prepend a column of ones.
#' @return List with estimate, posterior_mean_beta, posterior_sd_beta,
#'   sigma2_hat, ig_shape, ig_scale, posterior_mean_sigma2, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eqs. (6.1)-(6.2) p.172. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm042(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = 5L)
Msm042 <- function(X, y, add_intercept = TRUE) {
  f <- .gpolsfit(X, y, add_intercept = add_intercept)
  df <- length(.gpflat(y)) - length(f$beta)
  list(estimate = f$beta[1L], posterior_mean_beta = f$beta,
       posterior_sd_beta = f$se_beta, sigma2_hat = f$sigma2,
       ig_shape = df / 2, ig_scale = df * f$sigma2 / 2,
       posterior_mean_sigma2 = if (df > 2) (df * f$sigma2 / 2) / (df / 2 - 1) else NaN,
       method = "reference-prior Bayesian linear regression (MVSML 2022 eq. 6.1-6.2)")
}
