# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (5.1) p.142 (re-export)
#'
#' The same method as \code{Msm010()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm010()}.
#' @return The list returned by \code{Msm010()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' set.seed(11)
#' g <- rep(1:5, each = 4)
#' X <- cbind(1, rnorm(20)); Z <- model.matrix(~ factor(g) - 1)
#' y <- as.numeric(X %*% c(2, 1) + Z %*% rnorm(5, sd = 0.7) + rnorm(20))
#' Msm013(X, Z, y, D = diag(0.5, 5))$beta
#' @export
Msm013 <- function(...) Msm010(...)
