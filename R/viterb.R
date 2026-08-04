# SPDX-License-Identifier: AGPL-3.0-or-later
#' Most likely hidden state sequence, in one forward pass
#'
#' The greedy per-step choice is not the best path: a state that looks
#' good now can be unreachable from what follows. Viterbi carries, for
#' every state, the best path ending there, collapsing an exponential
#' search to O(T K^2). Log space, because a product of a thousand
#' probabilities underflows before the path is decided.
#'
#' Formula:
#' \code{delta_t(j) = max_i [delta_{t-1}(i) + log A_ij] + log B_j(o_t)}.
#'
#' @param obs Zero-based observation symbols.
#' @param trans Transition probabilities.
#' @param emit Emission probabilities.
#' @param init Initial state distribution; uniform by default.
#' @return List with \code{path}, \code{estimate}, \code{T}, \code{K}.
#' @references Viterbi, A. J. (1967). IEEE Trans Inform Theory
#'   13:260-269.
#' @export
Viterb <- function(obs, trans, emit, init = NULL) {
  o <- as.integer(round(as.numeric(obs))) + 1L
  A <- as.matrix(trans); B <- as.matrix(emit)
  K <- nrow(A); T_ <- length(o)
  NEG <- -1e300
  lg <- function(v) ifelse(v > 0, log(v), NEG)
  pi_ <- if (is.null(init)) rep(1 / K, K) else as.numeric(init)
  delta <- lg(pi_) + lg(B[, o[1]])
  psi <- matrix(0L, max(T_ - 1L, 1L), K)
  if (T_ > 1L) for (t in 2:T_) {
    nd <- numeric(K)
    for (j in seq_len(K)) {
      v <- delta + lg(A[, j])
      arg <- which.max(v)
      nd[j] <- v[arg] + lg(B[j, o[t]])
      psi[t - 1L, j] <- arg
    }
    delta <- nd
  }
  last <- which.max(delta)
  path <- integer(T_); path[T_] <- last
  if (T_ > 1L) for (t in seq(T_ - 1L, 1L)) {
    last <- psi[t, last]
    path[t] <- last
  }
  .t1_result(path = path - 1L, estimate = max(delta), T = T_, K = K,
             method = "Viterbi most-likely state path")
}
