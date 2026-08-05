# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (8.3) p.254 (re-export).
#'
#' The stub generator stamped several extracted page fragments with the same
#' function name, so the implementation lives once in \code{Msm128()} and this
#' module re-exports it. Calling either path runs the same code.
#'
#' @param ... Passed unchanged to \code{Msm128()}.
#' @return The list returned by \code{Msm128()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm130 <- function(...) Msm128(...)
