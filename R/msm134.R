# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (8.3) p.254 (re-export)
#'
#' The same method as \code{Msm128()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm128()}.
#' @return The list returned by \code{Msm128()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' xx <- seq(0, 1, length.out = 8); K <- exp(-outer(xx, xx, "-")^2 / 0.1)
#' Msm134(K, cos(2 * pi * xx), lam = 2)$estimate
#' @export
Msm134 <- function(...) Msm128(...)
