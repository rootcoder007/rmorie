# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normalised Laplacian of a weighted graph (internal)
#'
#' @param A Symmetric non-negative weight matrix.
#' @param who Caller name used in error messages.
#' @return List with \code{M}, \code{L}, \code{d}, \code{n}.
#' @keywords internal
.sgt_lapsym <- function(A, who) {
  M <- as.matrix(A)
  n <- nrow(M)
  if (n == 0L) stop(paste0(who, ": adjacency matrix is empty"))
  if (ncol(M) != n) stop(paste0(who, ": adjacency matrix must be square"))
  d <- rowSums(M)
  if (any(d <= 0)) stop(paste0(who, ": every node must have positive degree"))
  L <- diag(1, n) - M / sqrt(outer(d, d))
  list(M = M, L = L, d = d, n = n)
}

#' Laplacian eigenmaps: embed a graph in k dimensions
#'
#' The trivial eigenvector \code{D^{1/2} 1} at eigenvalue 0 carries no
#' geometry and is dropped; the next \code{k} are the embedding. On a
#' graph with \code{c} connected components eigenvalue 0 has multiplicity
#' \code{c}, so a disconnected graph gives coordinates constant per
#' component -- the method behaving correctly, not a defect.
#'
#' Formula: \code{L_sym = I - D^-1/2 A D^-1/2}; \code{Y} is columns
#' 2..k+1 of its eigenvectors, ordered by ascending eigenvalue.
#'
#' @param A Symmetric non-negative weight matrix.
#' @param k Embedding dimension, \code{1 <= k <= n - 1}.
#' @return List with \code{Y}, \code{eigvals}, \code{lambda1}, \code{k},
#'   \code{n}.
#' @references Belkin, M. & Niyogi, P. (2003). Laplacian eigenmaps for
#'   dimensionality reduction and data representation. Neural
#'   Computation 15(6):1373-1396. \doi{10.1162/089976603321780317}.
#' @export
Sgtlap2 <- function(A, k = 2) {
  s <- .sgt_lapsym(A, "Sgtlap2")
  n <- s$n
  k <- as.integer(k)
  if (k < 1L || k > n - 1L) stop("Sgtlap2: need 1 <= k <= n - 1")
  e <- .s03jacobi(s$L)
  Y <- e$vectors[, seq_len(k) + 1L, drop = FALSE]
  .t1_result(Y = Y, eigvals = e$values[seq_len(k) + 1L],
             lambda1 = e$values[1], k = k, n = n,
             method = "Laplacian eigenmaps on L_sym")
}
