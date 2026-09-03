# SPDX-License-Identifier: AGPL-3.0-or-later
#' Polynomial kernel matrix
#'
#' Formula: K(x_i, x_j) = (gamma * x_i'x_j + a)^d
#'
#' @param X One record per row.
#' @param degree Polynomial degree d.
#' @param gamma Scale on the inner product; None uses 1/p.
#' @param coef0 Constant a added before raising to the power d.
#' @param Z Second set of records; None gives the square Gram matrix of X.
#'
#' @return List with ``K``, ``degree``, ``gamma``, ``coef0``, ``n``, ``m``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.  Chapter 8, Sect. 8.2.2 pp. 255-256 and the worked
#' degree-2 example on p. 256, where the polynomial kernel of degree d with constant a is
#' (gamma x_i'x_j + a)^d and the feature-space dimension is discussed on p. 261.  Read
#' from the chapter PDF, not recalled.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Polykern(V)
Polykern <- function(X, degree = 2, gamma = NULL, coef0 = 1, Z = NULL) {
  K <- morie_kernel_matrix(X, kernel = "polynomial", gamma = gamma,
                           degree = as.integer(degree),
                           coef0 = as.numeric(coef0), Z = Z)
  p <- ncol(as.matrix(X))
  g <- if (is.null(gamma)) 1 / p else as.numeric(gamma)
  .t1_result(K = K, degree = as.integer(degree), gamma = g,
             coef0 = as.numeric(coef0), n = nrow(K), m = ncol(K),
             method = "Polynomial kernel, MVSML Sect. 8.2.2")
}
