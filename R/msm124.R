# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (8.1) p.253 (re-export)
#'
#' The same method as \code{Msm123()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm123()}.
#' @return The list returned by \code{Msm123()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' xx <- seq(0, 1, length.out = 8); K <- exp(-outer(xx, xx, "-")^2 / 0.1)
#' Msm124(K, sin(2 * pi * xx), beta = rep(0.1, 8), lam = 0.1)$estimate
#' @export
Msm124 <- function(...) Msm123(...)
