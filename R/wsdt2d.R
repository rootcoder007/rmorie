# SPDX-License-Identifier: AGPL-3.0-or-later
#' Transport distance between equal-size clouds by optimal assignment
#'
#' Above one dimension there is no sorting shortcut, so the coupling is
#' an assignment problem. With equal-size samples the optimal coupling is
#' a permutation, found here by the Hungarian method -- exact and
#' deterministic, unlike entropic approximations that depend on a
#' regularisation parameter and a stopping rule.
#'
#' Formula: \code{W_p^p = min_sigma (1/n) sum_i ||x_i - y_sigma(i)||^p}.
#'
#' @param X_samples,Y_samples Equal-size point clouds.
#' @param p Order.
#' @return List with \code{estimate}, \code{wpp}, \code{assignment}, \code{n}.
#' @references Villani, C. (2009). Optimal Transport: Old and New,
#'   Springer Grundlehren 338, ch 6; Kuhn, H. W. (1955) NRLQ 2:83-97.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Wsdt2d(V, V)
Wsdt2d <- function(X_samples, Y_samples, p = 2) {
  A <- as.matrix(X_samples); B <- as.matrix(Y_samples); n <- nrow(A)
  Cst <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    Cst[i, j] <- sum(abs(A[i, ] - B[j, ])^2)^(p / 2)
  }
  asg <- .s4_hungarian(Cst)
  tot <- sum(vapply(seq_len(n), function(i) Cst[i, asg[i] + 1L], 0)) / n
  .t1_result(estimate = tot^(1 / p), wpp = tot, assignment = asg, n = n,
             method = "Wasserstein-p by optimal assignment")
}
