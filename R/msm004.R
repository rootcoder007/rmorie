# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (1.3) p.16 (re-export)
#'
#' The same method as \code{Msm003()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm003()}.
#' @return The list returned by \code{Msm003()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' Msm004(list(c(4.1, 4.5, 3.9), c(5.2, 5.0, 5.6), c(4.8, 4.4, 4.9)))$beta
#' @export
Msm004 <- function(...) Msm003(...)
