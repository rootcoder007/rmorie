# SPDX-License-Identifier: AGPL-3.0-or-later
#' eq. (7.5) p.221 (re-export)
#'
#' The same method as \code{Msm098()}, re-exported under this name; both run the same
#' code.
#'
#' @param ... Passed unchanged to \code{Msm098()}.
#' @return The list returned by \code{Msm098()}.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer. DOI 10.1007/978-3-030-89010-0.
#' @examples
#' set.seed(17)
#' G <- crossprod(matrix(rnorm(16), 4)) / 4 + diag(0.1, 4)
#' Msm102(n = 4L, X_E = model.matrix(~ factor(c(1, 1, 2, 2)) - 1), Z_L = diag(4), L_g = t(chol(G)))$estimate
#' @export
Msm102 <- function(...) Msm098(...)
