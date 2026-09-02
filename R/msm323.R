# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric links of the zero-altered Poisson forest.
#'
#' Formula: log(mu) = f_mu(x) and log(theta/(1-theta)) = f_theta(x)
#' (eq. 15.1): under ZAP_RF and ZAPC_RF the links between covariates and
#' response are general nonparametric functions estimated by two random
#' forests rather than linear predictors.
#'
#' @param mu_pred Forest prediction on the log-mean scale.
#' @param theta_pred Forest prediction on the logit scale.
#' @return List with estimate, mu, theta, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (15.1) p.651. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Msm323(V, V)
Msm323 <- function(mu_pred, theta_pred) {
  f <- .gpzaplink(mu_pred, theta_pred)
  list(estimate = f$mu, mu = f$mu, theta = f$theta,
       method = "ZAP nonparametric links (MVSML 2022 eq. 15.1)")
}
