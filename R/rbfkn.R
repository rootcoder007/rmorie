# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian (radial basis) kernel matrix.
#'
#' Formula: K(x_i, x_j) = exp(-gamma * ||x_i - x_j||^2)
#'
#' @param X One record per row.
#' @param gamma Bandwidth; None uses 1/p, p the number of columns of X.
#' @param Z Second set of records; None gives the square Gram matrix of X.
#'
#' @return List with ``K``, ``gamma``, ``n``, ``m``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 8, Sect. 8.2.2 pp. 263-264.  The book prints the Gaussian kernel through its R implementation K.radial, which computes exp(-gamma * ||x1 - x2||^2); gamma, not a variance-style bandwidth, is the parameter the text discusses on p. 264.  The placeholder this replaced carried a 1/(2h^2) parameterization that the book does not use.  Read from the chapter PDF, not recalled.
#' @export
Rbfkern <- function(X, gamma = NULL, Z = NULL) {
  K <- morie_kernel_matrix(X, kernel = "gaussian", gamma = gamma, Z = Z)
  p <- ncol(as.matrix(X))
  g <- if (is.null(gamma)) 1 / p else as.numeric(gamma)
  .t1_result(K = K, gamma = g, n = nrow(K), m = ncol(K),
             method = "Gaussian (RBF) kernel, MVSML Sect. 8.2.2")
}
