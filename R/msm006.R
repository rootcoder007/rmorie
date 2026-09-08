# SPDX-License-Identifier: AGPL-3.0-or-later
#' One-way random-effects (mixed) model
#'
#' Formula: GY_ij = beta + b_i + e_ij with b_i ~ N(0, sigma2_b) (eq. 1.5).
#' Balanced ANOVA estimators: sigma2_e = MSE, sigma2_b = (MSB - MSE)/r.
#'
#' @param groups List of equal-length observation vectors, one per level.
#' @return List with estimate, beta, sigma2_b, sd_residual, icc, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (1.5) p.18. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Msm006(D)
Msm006 <- function(groups) {
  s <- .gponeway(groups)
  list(estimate = s$grand_mean, beta = s$grand_mean, sigma2_b = s$sigma2_b,
       sd_residual = s$sd_residual, icc = s$icc,
       method = "one-way random effects (MVSML 2022 eq. 1.5)")
}
