# SPDX-License-Identifier: AGPL-3.0-or-later
#' Non-informative prior for the linear model.
#'
#' Formula: f(beta, sigma2) proportional to sigma^-2 (eq. 6.2), flat in beta
#' and in log(sigma). It is improper because it does not integrate to one, yet
#' yields a proper posterior whenever X has full column rank. The density is
#' returned up to its (infinite) normalizing constant.
#'
#' @param sigma2 Error variance, strictly positive.
#' @param beta Regression coefficients; the prior is flat in beta so this is
#'   accepted for signature compatibility and does not enter the density.
#' @return List with estimate, density, log_density, proper, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (6.2) p.172. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm043 <- function(sigma2, beta = NULL) {
  s2 <- as.numeric(sigma2)
  if (s2 <= 0) stop("sigma2 must be positive")
  dens <- s2^-2
  list(estimate = dens, density = dens, log_density = -2 * log(s2),
       proper = FALSE, method = "non-informative prior (MVSML 2022 eq. 6.2)")
}
