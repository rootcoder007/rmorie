# SPDX-License-Identifier: AGPL-3.0-or-later
#' Expected steps for a simple random walk to reach a target vertex.
#'
#' Formula: with \eqn{P_{ij} = w_{ij}/\sum_k w_{ik}},
#' \eqn{H(t,t) = 0} and \eqn{H(i,t) = 1 + \sum_j P_{ij} H(j,t)} for
#' \eqn{i \ne t} -- a linear system of size n-1, solved exactly rather
#' than by simulating the walk.  Vertices from which the target is
#' unreachable have infinite hitting time and are reported as such
#' instead of being dropped or given a large finite number; the system
#' is solved only over the vertices that can reach it.  Hitting time is
#' not symmetric, which is why the commute time H(i,j) + H(j,i) is the
#' quantity with metric behaviour.
#'
#' @param G n by n non-negative weight matrix; zero means no edge.
#' @param start Starting vertex, 0-based to match the Python arm;
#'   selects the scalar reported as \code{estimate}.  All vertices are
#'   returned regardless.
#' @param target Vertex to be hit, 0-based.
#' @return List with \code{estimate}, \code{hitting}, \code{target},
#'   \code{start}, \code{n}, \code{method}.
#' @references Lovasz (1996), Random walks on graphs: a survey, in Combinatorics, Paul Erdos is Eighty, vol. 2, pp. 353-398.  The PDF on Lovasz's ELTE page could not be fetched from this host (expired TLS certificate on web.cs.elte.hu), so this is the standard first-step recurrence rather than a quoted equation.  It is anchored in the harness on the cycle C_n, where the classical closed form H(i,j) = d(n-d) with d the cyclic distance holds exactly and is independent of this code.
#' @export
Hittime <- function(G, start = NULL, target = 0L) {
  W <- as.matrix(G); n <- nrow(W)
  if (n < 2L || ncol(W) != n) stop("G must be a square weight matrix with n >= 2")
  if (any(W < 0)) stop("weights must be non-negative")
  target0 <- as.integer(target)
  if (target0 < 0L || target0 >= n) stop("target out of range")
  if (is.null(start)) start <- if (target0 != 0L) 0L else 1L
  start0 <- as.integer(start)
  if (start0 < 0L || start0 >= n) stop("start out of range")
  target <- target0 + 1L; start <- start0 + 1L
  reach <- rep(FALSE, n); reach[target] <- TRUE
  queue <- target; head <- 1L
  while (head <= length(queue)) {
    v <- queue[head]; head <- head + 1L
    for (u in seq_len(n)) if (!reach[u] && W[u, v] > 0) { reach[u] <- TRUE; queue <- c(queue, u) }
  }
  idx <- which(reach & seq_len(n) != target)
  H <- rep(Inf, n); H[target] <- 0
  if (length(idx) > 0L) {
    k <- length(idx)
    A <- matrix(0, k, k); b <- rep(1, k)
    pos <- integer(n); pos[idx] <- seq_len(k)
    for (r in seq_len(k)) {
      i <- idx[r]; deg <- sum(W[i, ])
      if (deg <= 0) stop(sprintf("vertex %d reaches the target but has no outgoing weight", i))
      A[r, r] <- 1
      for (j in seq_len(n)) {
        if (W[i, j] <= 0 || j == target) next
        if (pos[j] > 0L) A[r, pos[j]] <- A[r, pos[j]] - W[i, j] / deg
      }
    }
    H[idx] <- as.numeric(solve(A, b))
  }
  .t4_result(estimate = H[start], hitting = H, target = target0,
             start = start0, n = as.integer(n),
             method = "Expected hitting time of a simple random walk")
}
