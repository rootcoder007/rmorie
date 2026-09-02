# SPDX-License-Identifier: AGPL-3.0-or-later
#' Transport in a feature space, with the cost read off a kernel
#'
#' Once the ground cost is the squared distance in a feature space, it
#' never has to be formed there: \code{||phi(x) - phi(y)||^2 = k(x,x) +
#' k(y,y) - 2 k(x,y)}, so the kernel alone determines the cost, which is
#' the \code{-2 k(x,y)} of the usual shorthand plus the two diagonal terms
#' that keep it non-negative. Dropping them would shift every entry by a
#' row and a column constant, which leaves the optimal plan alone but
#' makes the reported cost meaningless.
#'
#' Formula: \code{C_ij = k(x_i,x_i) + k(y_j,y_j) - 2 k(x_i,y_j)}, then
#' \code{T} by Sinkhorn and \code{EMD_approx = <T, C>} -- Genevay, Peyre
#' and Cuturi (2018), Section 3.
#'
#' @param X,Y Two point clouds, given uniform weight.
#' @param kernel Either \code{"gaussian"} or \code{"linear"}.
#' @param epsilon Entropic strength, positive.
#' @param gamma Gaussian bandwidth parameter.
#' @param max_iter Sinkhorn sweeps.
#' @return List with \code{EMD_approx}, \code{C}, \code{exact_cost},
#'   \code{n}, \code{m}.
#' @references Genevay, A., Peyre, G. and Cuturi, M. (2018). Proceedings
#'   of Machine Learning Research 84:1608-1617 (AISTATS).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Otker(V, V)
Otker <- function(X, Y, kernel = "gaussian", epsilon = 0.1, gamma = 1,
                  max_iter = 200) {
  A <- as.matrix(X); B <- as.matrix(Y)
  if (ncol(A) != ncol(B)) stop("point clouds must share a dimension")
  n <- nrow(A); m <- nrow(B)
  gram <- function(P, Q) {
    if (kernel == "linear") return(P %*% t(Q))
    if (kernel == "gaussian") {
      D <- .ot_costmat(P, Q, 2)
      return(exp(-as.numeric(gamma) * D))
    }
    stop("kernel must be 'gaussian' or 'linear'")
  }
  Kxy <- gram(A, B)
  kxx <- diag(as.matrix(gram(A, A)))
  kyy <- diag(as.matrix(gram(B, B)))
  C <- outer(kxx, kyy, "+") - 2 * Kxy
  a <- rep(1 / n, n); b <- rep(1 / m, m)
  s <- .ot_sinkhorn(a, b, C, as.numeric(epsilon), max_iter)
  ex <- .ot_emd(a, b, C)
  .t1_result(EMD_approx = sum(s$T * C), C = C, exact_cost = ex$cost,
             n = n, m = m, method = "Kernel-induced transport cost")
}
