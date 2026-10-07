# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (15.2) p.651 (re-export)
#'
#' The same method as \code{Msm325()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm325()}.
#' @return The list returned by \code{Msm325()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' Msm326(y_positive = c(1, 2, 1, 3, 4, 2, 6, 5))$estimate
#' @export
Msm326 <- function(...) Msm325(...)
