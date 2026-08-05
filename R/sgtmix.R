# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mixing time of a random walk from the absolute spectral gap
#'
#' The walk matrix \code{P = D^-1 A} is similar to
#' \code{D^-1/2 A D^-1/2}, so its spectrum is real and comes from a
#' symmetric eigenproblem. The bound uses the ABSOLUTE spectral gap
#' \code{gamma* = 1 - max_{i>=2} |lambda_i|}: a bipartite graph has
#' \code{lambda_n = -1} and never mixes, which the signed gap misses.
#'
#' Formula: \code{tau_mix(eps) <= log(1 / eps) / gamma*}.
#'
#' @param A Symmetric non-negative adjacency; positive row sums.
#' @param epsilon Total-variation target in (0, 1).
#' @return List with \code{tau_mix}, \code{estimate}, \code{gap},
#'   \code{slem}, \code{n}.
#' @references Levin, D. A., Peres, Y. & Wilmer, E. L. (2017). Markov
#'   Chains and Mixing Times, 2nd edition, American Mathematical Society;
#'   Theorem 12.4.
#' @export
Sgtmix <- function(A, epsilon = 0.01) {
  M <- as.matrix(A)
  n <- nrow(M)
  if (n == 0L) stop("Sgtmix: adjacency matrix is empty")
  if (ncol(M) != n) stop("Sgtmix: adjacency matrix must be square")
  eps <- as.numeric(epsilon)
  if (!(eps > 0 && eps < 1)) stop("Sgtmix: epsilon must lie in (0, 1)")
  d <- rowSums(M)
  if (any(d <= 0)) stop("Sgtmix: every node must have positive degree")
  S <- M / sqrt(outer(d, d))
  ev <- .s03jacobi(S)$values
  slem <- max(abs(ev[seq_len(n - 1L)]))
  gap <- 1 - slem
  tau <- if (gap > 0) log(1 / eps) / gap else Inf
  .t1_result(tau_mix = tau, estimate = tau, gap = gap, slem = slem, n = n,
             method = "Relaxation-time mixing bound, log(1/eps)/gamma*")
}
