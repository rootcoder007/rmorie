# SPDX-License-Identifier: AGPL-3.0-or-later
#' Single-mean model for a one-way layout
#'
#' Formula: GY_ij = beta + e_ij (eq. 1.2), one grand mean for all levels.
#'
#' @param groups List of equal-length observation vectors, one per level.
#' @return List with estimate, beta, sd_residual, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (1.2) p.9. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' # one grand mean for every level of the factor
#' Msm002(list(c(4.1, 4.5, 3.9), c(5.2, 5.0, 5.6), c(4.8, 4.4, 4.9)))$beta
#' @export
Msm002 <- function(groups) {
  .morie_arg(groups, "l")
  s <- .gponeway(groups)
  list(estimate = s$grand_mean, beta = s$grand_mean,
       sd_residual = s$sd_single_mean,
       method = "single-mean model (MVSML 2022 eq. 1.2)")
}
