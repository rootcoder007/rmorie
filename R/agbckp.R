# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero MCTS backup along the simulation path
#'
#' Schrittwieser et al. (2020), arXiv:1911.08265 (FETCHED), appendix B
#' ("Backup"), and Silver et al. (2017), Nature 550, 354-359: for every
#' edge on the path, N <- N + 1, W <- W + G, Q <- W / N.  In a two-player
#' zero-sum game the value is negated at each ply as it propagates, since
#' a value is always from the point of view of the player to move; that
#' is the `alternate` flag, and it is what makes the backup a minimax
#' backup rather than a plain average.  MuZero's general form adds
#' discounting and the intermediate rewards r_k, accepted as `rewards`.
#'
#' @param leaf the expanded leaf; carried through untouched.
#' @param value the network value at the leaf.
#' @param path the edges root-first; only its length is used.
#' @param N,W current edge statistics, root-first.
#' @param rewards MuZero intermediate rewards per edge.
#' @param gamma discount per ply.
#' @param alternate negate the value each ply (two-player zero-sum).
#' @return list: estimate, n, w, q, g, leaf, method.
#' @keywords internal
#' @examples
#' Mctsbackup(0, 0.5, 1:3)$q
#' @export
Mctsbackup <- function(leaf, value, path, N = NULL, W = NULL, rewards = NULL,
                       gamma = 1, alternate = TRUE) {
  L <- length(path)
  n <- if (!is.null(N)) .s03vec(N) else numeric(L)
  w <- if (!is.null(W)) .s03vec(W) else numeric(L)
  r <- if (!is.null(rewards)) .s03vec(rewards) else numeric(L)
  g <- numeric(L)
  acc <- as.numeric(value)
  if (L > 0L) for (i in seq(L, 1L)) {
    if (alternate) acc <- -acc
    acc <- r[i] + as.numeric(gamma) * acc
    g[i] <- acc
    n[i] <- n[i] + 1
    w[i] <- w[i] + acc
  }
  q <- numeric(L)
  if (L > 0L) for (i in seq_len(L)) q[i] <- if (n[i] > 0) w[i] / n[i] else 0
  list(estimate = if (L > 0L) q[1] else NaN, n = n, w = w, q = q, g = g,
       leaf = leaf,
       method = "AlphaZero MCTS backup along the simulation path")
}
