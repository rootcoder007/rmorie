# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (15.3) p.652 (re-export)
#'
#' The same method as \code{Msm327()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm327()}.
#' @return The list returned by \code{Msm327()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' Msm328(theta_hat = 0.6, mu_hat = 1)$estimate
#' @export
Msm328 <- function(...) Msm327(...)
