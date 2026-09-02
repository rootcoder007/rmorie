# SPDX-License-Identifier: AGPL-3.0-or-later
#' Squared RKHS norm of a kernel expansion.
#'
#' Formula: ||f||_H^2 = sum_i sum_j beta_i beta_j K(x_i, x_j) = beta' K beta
#'
#' @param beta Kernel expansion coefficients, length n.
#' @param K Gram matrix.
#'
#' @return List with ``norm2``, ``norm``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 8, Eq. (8.2) p. 254: the squared norm of f in the reproducing kernel Hilbert space is beta'K beta.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' Rkhsnorm(beta = 0.5, K = 5L)
Rkhsnorm <- function(beta, K) {
  n2 <- morie_rkhs_norm(beta, K)
  if (n2 < 0) stop("beta'K beta is negative: K is not positive semi-definite")
  .t1_result(norm2 = n2, norm = sqrt(n2), n = length(as.numeric(beta)),
             method = "Squared RKHS norm, MVSML Eq. (8.2)")
}
