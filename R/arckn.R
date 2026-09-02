# SPDX-License-Identifier: AGPL-3.0-or-later
#' Arc-cosine kernel matrix
#'
#' Formula: K(x_i, x_j) = (1/pi) * ||x_i|| * ||x_j|| * \[sin(theta) + (pi - theta) cos(theta)\],  theta = angle(x_i, x_j)
#'
#' @param X One record per row.
#' @param Z Second set of records; None gives the square Gram matrix of X.
#' @param depth Number of times the kernel is composed with itself.
#'
#' @return List with ``K``, ``depth``, ``n``, ``m``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 8, Sect. 8.2.2, the arc-cosine kernel named on p. 252 and developed later in the chapter; the implementation delegates to the chapter-8 arc-cosine kernel already verified against the book for this shelf.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Arckern(V)
Arckern <- function(X, Z = NULL, depth = 1L) {
  out <- morie_arccos_kernel(X, Z = Z, depth = as.integer(depth))
  K <- if (is.list(out)) out$K else out
  .t1_result(K = K, depth = as.integer(depth), n = nrow(K), m = ncol(K),
             method = "Arc-cosine kernel, MVSML Chap. 8")
}
