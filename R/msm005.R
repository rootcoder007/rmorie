# SPDX-License-Identifier: AGPL-3.0-or-later
#' Reparameterized one-way model.
#'
#' Formula: GY_ij = beta-bar + (beta_i - beta-bar) + e_ij (eq. 1.4), the
#' fixed-effects fit rewritten around the average level.
#'
#' @param groups List of equal-length observation vectors, one per level.
#' @return List with estimate, beta_bar, deviations, deviations_sum, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (1.4) p.17. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Msm005(D)
Msm005 <- function(groups) {
  s <- .gponeway(groups)
  list(estimate = s$grand_mean, beta_bar = s$grand_mean,
       deviations = s$deviations, deviations_sum = sum(s$deviations),
       method = "reparameterized one-way model (MVSML 2022 eq. 1.4)")
}
