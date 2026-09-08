# SPDX-License-Identifier: AGPL-3.0-or-later
#' Exponential (Laplace) kernel matrix
#'
#' Formula: K(x_i, x_j) = exp(-gamma * ||x_i - x_j||)
#'
#' @param X One record per row.
#' @param gamma Bandwidth; None uses 1/p.
#' @param Z Second set of records; None gives the square Gram matrix of X.
#'
#' @return List with ``K``, ``gamma``, ``n``, ``m``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.  Chapter 8, Sect. 8.2.2 p. 264: the book calls this the
#' Exponential Kernel, K(x_i, x_j) = exp(-gamma ||x_i - x_j||), and notes it is close to
#' the Gaussian kernel.  NOTE: the placeholder this replaced was named for the Laplacian
#' kernel; the book's display uses the Euclidean norm, not the L1 norm that the name
#' Laplacian kernel usually implies, and the book's form is what is implemented.  Read
#' from the chapter PDF, not recalled.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Expkern(V)
Expkern <- function(X, gamma = NULL, Z = NULL) {
  K <- morie_kernel_matrix(X, kernel = "exponential", gamma = gamma, Z = Z)
  p <- ncol(as.matrix(X))
  g <- if (is.null(gamma)) 1 / p else as.numeric(gamma)
  .t1_result(K = K, gamma = g, n = nrow(K), m = ncol(K),
             method = "Exponential (Laplace) kernel, MVSML Sect. 8.2.2")
}
