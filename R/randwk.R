# SPDX-License-Identifier: AGPL-3.0-or-later
#' Distribution of a simple random walk after a fixed number of steps
#'
#' Simulating the walk would make the two language arms agree only in
#' distribution. The transition law is linear, so the whole distribution
#' is pushed forward exactly -- same numbers in both arms and no Monte
#' Carlo error.
#'
#' Formula: \code{P(j | i) = A_ij / k_i}; the returned vector is
#' \code{e_start P^steps}.
#'
#' @param G Non-negative weight matrix with positive row sums.
#' @param start Starting node, 1-based here; the Python arm takes it
#'   0-based. Reported indices are 0-based in both arms.
#' @param steps Number of steps, non-negative.
#' @return List with \code{p}, \code{estimate}, \code{argmax} (0-based),
#'   \code{p_start}, \code{n}.
#' @references Lovasz, L. (1996). Random walks on graphs: a survey. In
#'   Combinatorics, Paul Erdos is Eighty, Vol. 2, pages 1-46, Janos
#'   Bolyai Mathematical Society, Budapest.
#' @export
Randwk <- function(G, start = 1L, steps = 1L) {
  M <- as.matrix(G)
  n <- nrow(M)
  if (n == 0L) stop("Randwk: graph is empty")
  if (ncol(M) != n) stop("Randwk: graph must be square")
  start <- as.integer(start)
  steps <- as.integer(steps)
  if (start < 1L || start > n) stop("Randwk: start is outside the graph")
  if (steps < 0L) stop("Randwk: steps must be non-negative")
  if (any(M < 0)) stop("Randwk: weights must be non-negative")
  d <- rowSums(M)
  if (any(d <= 0)) stop("Randwk: every node must have positive degree")
  P <- M / d
  p <- numeric(n)
  p[start] <- 1
  for (s in seq_len(steps)) p <- as.numeric(p %*% P)
  am <- which.max(p)
  .t1_result(p = p, estimate = p[am], argmax = am - 1L, p_start = p[start],
             n = n, method = "Exact random-walk law e_start P^steps")
}
