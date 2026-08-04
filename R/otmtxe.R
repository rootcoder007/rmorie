# SPDX-License-Identifier: AGPL-3.0-or-later
#' RAS matrix scaling to prescribed margins.
#'
#' Formula: M = diag(u) K diag(v) with u <- r / (K v) and v <- c / (K' u), alternated
#'
#' @param K Non-negative kernel matrix.
#' @param row_target Required row sums.
#' @param col_target Required column sums.
#' @param max_iter Fixed number of alternations.

#' @return List with ``M``, ``u``, ``v``, ``row_error``, ``col_error``, ``iterations``.
#' @references Bregman (1967), The relaxation method of finding the common point of convex sets, USSR Computational Mathematics and Mathematical Physics 7:200-217. Not held locally; alternating diagonal scaling to fixed margins (RAS, Sinkhorn-Knopp) is the standard published form of the method.
#' @export
Rasscale <- function(K, row_target, col_target, max_iter = 200) {
  K <- as.matrix(K); r <- .t1_vec(row_target); c <- .t1_vec(col_target)
  m <- nrow(K); n <- ncol(K)
  if (length(r) != m || length(c) != n) stop("targets must match the shape of K")
  if (any(K < 0)) stop("K must be non-negative")
  if (abs(sum(r) - sum(c)) > 1e-9 * max(1, abs(sum(r))))
    stop("row and column targets must have the same total")
  u <- rep(1, m); v <- rep(1, n)
  for (it in seq_len(as.integer(max_iter))) {
    s <- as.numeric(K %*% v); u <- ifelse(s > 0, r / s, 0)
    s2 <- as.numeric(t(K) %*% u); v <- ifelse(s2 > 0, c / s2, 0)
  }
  M <- diag(u, m) %*% K %*% diag(v, n)
  .t1_result(M = M, u = u, v = v,
             row_error = max(abs(rowSums(M) - r)),
             col_error = max(abs(colSums(M) - c)),
             iterations = as.integer(max_iter), method = "RAS matrix scaling")
}
