# SPDX-License-Identifier: AGPL-3.0-or-later

#' Effective resistance between nodes
#'
#' Formula: R_uv = (e_u - e_v)' L^+ (e_u - e_v), the resistance distance
#' of Klein & Randic (1993), where L = D - A is the graph Laplacian and
#' L^+ its Moore-Penrose inverse.
#'
#' Rather than forming L^+ the linear system is grounded: L is singular
#' with the all-ones vector in its kernel, so deleting row/column
#' \code{v} gives a nonsingular matrix, and the solution x of the
#' grounded system against e_u satisfies R_uv = x_u.  Exact, and no
#' pseudoinverse is needed.
#'
#' @param G Symmetric non-negative weight (or 0/1 adjacency) matrix;
#'   weights are read as CONDUCTANCES, per Klein & Randic.
#' @param u,v Zero-based node indices in the same component.
#' @return List with \code{estimate}, \code{resistance},
#'   \code{degree_u}, \code{degree_v}, \code{n}, \code{method}.
#' @references Klein & Randic (1993), Journal of Mathematical Chemistry
#'   12(1):81-95, doi:10.1007/BF01164627.
#' @export
Esumtv <- function(G, u, v) {
  A <- .s03mat(G)
  n <- nrow(A)
  if (n == 0L) stop("empty input: G has no nodes")
  if (ncol(A) != n) stop("G must be square")
  if (any(A < 0)) stop("weights must be non-negative")
  if (max(abs(A - t(A))) > 1e-12) stop("G must be symmetric")
  u <- as.integer(u); v <- as.integer(v)
  if (u < 0L || u >= n || v < 0L || v >= n)
    stop("u and v must be valid node indices")
  deg <- rowSums(A) - diag(A)
  if (u == v)
    return(.t1_result(estimate = 0, resistance = 0, degree_u = deg[u + 1L],
                      degree_v = deg[v + 1L], n = n,
                      method = "Effective resistance between nodes"))
  L <- diag(deg, n) - (A - diag(diag(A), n))
  keep <- setdiff(seq_len(n), v + 1L)
  Lg <- L[keep, keep, drop = FALSE]
  b <- as.numeric(keep == (u + 1L))
  x <- tryCatch(.s03cholsolve(Lg, b),
                error = function(e) stop("u and v are not connected"))
  R <- x[which(keep == (u + 1L))]
  .t1_result(estimate = R, resistance = R, degree_u = deg[u + 1L],
             degree_v = deg[v + 1L], n = n,
             method = "Effective resistance between nodes")
}
