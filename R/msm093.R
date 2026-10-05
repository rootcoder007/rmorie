# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (7.3) p.219 (re-export)
#'
#' The same method as \code{Msm092()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm092()}.
#' @return The list returned by \code{Msm092()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' set.seed(16)
#' XE <- model.matrix(~ factor(rep(1:2, 5)) - 1)
#' X <- matrix(rnorm(20), 10, 2)
#' Msm093(n = 10, X_E = XE, X = X)$estimate
#' @export
Msm093 <- function(...) Msm092(...)
