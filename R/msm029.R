# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (5.5) p.153 (re-export)
#'
#' The same method as \code{Msm026()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm026()}.
#' @return The list returned by \code{Msm026()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' set.seed(12)
#' J <- 6; nT <- 2
#' G <- crossprod(matrix(rnorm(J * J), J)) / J
#' Y <- matrix(rnorm(J * nT, 5), J, nT)
#' Msm029(Y, Z = diag(J), G = G, Sigma_T = diag(nT), R_T = diag(nT))$mu
#' @export
Msm029 <- function(...) Msm026(...)
