# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (15.4) p.652 (re-export)
#'
#' The same method as \code{Msm329()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm329()}.
#' @return The list returned by \code{Msm329()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' Msm330(theta_hat = 0.3, mu_hat = 4, threshold = 0.5)$estimate
#' @export
Msm330 <- function(...) Msm329(...)
