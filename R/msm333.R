# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (3.1) p.71 with the OLS solution pp.72-73 (re-export)
#'
#' The stub generator stamped several extracted page fragments with the same
#' function name, so the implementation lives once in \code{Msm332()} and this
#' module re-exports it. Calling either path runs the same code.
#'
#' @param ... Passed unchanged to \code{Msm332()}.
#' @return The list returned by \code{Msm332()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm333 <- function(...) Msm332(...)
