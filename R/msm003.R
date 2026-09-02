# SPDX-License-Identifier: AGPL-3.0-or-later
#' One-way fixed-effects model.
#'
#' Formula: GY_ij = beta_i + e_ij (eq. 1.3), a separate fixed effect per level.
#'
#' @param groups List of equal-length observation vectors, one per level.
#' @return List with estimate, beta, sd_residual, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (1.3) p.16. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Msm003(D)
Msm003 <- function(groups) {
  s <- .gponeway(groups)
  list(estimate = s$group_means[1L], beta = s$group_means,
       sd_residual = s$sd_residual,
       method = "one-way fixed effects (MVSML 2022 eq. 1.3)")
}
