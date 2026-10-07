# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (6.1)-(6.2) pp.172 (re-export)
#'
#' The same method as \code{Msm042()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm042()}.
#' @return The list returned by \code{Msm042()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' set.seed(13)
#' X <- matrix(rnorm(60), 30, 2)
#' y <- 1 + X %*% c(0.5, -1) + rnorm(30)
#' Msm045(X, as.numeric(y))$posterior_sd_beta
#' @export
Msm045 <- function(...) Msm042(...)
