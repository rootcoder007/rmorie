# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (3.1) p.71 with the OLS solution pp.72-73 (re-export)
#'
#' The same method as \code{Msm332()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm332()}.
#' @return The list returned by \code{Msm332()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' set.seed(29)
#' X <- matrix(rnorm(60), 30, 2)
#' y <- 1 + X %*% c(2, -1) + rnorm(30, sd = 0.1)
#' Msm333(X, as.numeric(y))$estimate
#' @export
Msm333 <- function(...) Msm332(...)
