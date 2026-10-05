# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (7.7) p.226 (re-export)
#'
#' The same method as \code{Msm109()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm109()}.
#' @return The list returned by \code{Msm109()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' set.seed(18)
#' X <- matrix(rnorm(40), 20, 2); y <- sample(0:2, 20, TRUE)
#' Msm117(X, y, beta0 = c(0, 0), beta = matrix(0, 2, 2))$estimate
#' @export
Msm117 <- function(...) Msm109(...)
