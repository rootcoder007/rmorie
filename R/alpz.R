# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero's MCTS with a neural prior
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED), whose search is
#' stated to be "identical to AlphaGo Zero" (Silver et al., Nature 550,
#' 354-359), and Schrittwieser et al. (2020), arXiv:1911.08265 (FETCHED),
#' appendix B, which prints all three phases.  Select descends by
#' argmax_a Q(s,a) + c_puct P(s,a) sqrt(sum_b N(s,b)) / (1 + N(s,a)) --
#' the c2 -> infinity limit of MuZero's rule; expand evaluates (p, v) once
#' at the new leaf and sets N = W = Q = 0; backup walks the path back
#' updating N, W and Q, negating the value each ply in a two-player game.
#' The search policy is the normalised root visit count.
#'
#' Determinism: a fixed simulation budget, never wall-clock; ties break to
#' the lowest action index; root Dirichlet noise must be passed in.
#' States are identified by a scalar id: step(s, a) returns the successor
#' id and net(s) returns list(p, v).
#'
#' @param state root state id.
#' @param net function s -> list(p, v).
#' @param num_sim number of simulations.
#' @param step function (s, a) -> successor id; NULL means a one-ply search.
#' @param c_puct exploration constant.
#' @param max_depth depth cap; 1 when step is NULL, else 64.
#' @param terminal optional function s -> logical.
#' @param alternate negate the value each ply.
#' @param root_noise optional Dirichlet vector to mix into the root prior.
#' @param eps mixing weight for root_noise.
#' @return list: estimate, action, pi, n, q, p, value, n_nodes, method.
#' @keywords internal
#' @examples
#' Azsearch(0, function(s) list(c(0.6, 0.4), 0.1), 8)$pi
#' @export
Azsearch <- function(state, net, num_sim, step = NULL, c_puct = 1.25,
                     max_depth = NULL, terminal = NULL, alternate = TRUE,
                     root_noise = NULL, eps = 0.25) {
  if (is.null(max_depth)) max_depth <- if (is.null(step)) 1L else 64L
  ids <- list(); P <- list(); N <- list(); W <- list(); V <- numeric(0)
  expand <- function(s) {
    out <- net(s)
    if (is.list(out) && length(out) == 2L) {
      p <- .s03vec(out[[1]]); v <- as.numeric(out[[2]])
    } else {
      p <- .s03vec(out); v <- 0
    }
    tot <- 0
    for (x in p) tot <- tot + x
    if (tot > 0) p <- p / tot
    ids[[length(ids) + 1L]] <<- s
    P[[length(P) + 1L]] <<- p
    N[[length(N) + 1L]] <<- numeric(length(p))
    W[[length(W) + 1L]] <<- numeric(length(p))
    V[[length(V) + 1L]] <<- v
    length(ids)
  }
  find <- function(s) {
    for (i in seq_along(ids)) if (identical(ids[[i]], s)) return(i)
    -1L
  }
  root <- expand(state)
  if (!is.null(root_noise)) {
    et <- .s03vec(root_noise)
    tot <- 0
    for (x in et) tot <- tot + x
    if (tot > 0) et <- et / tot
    e <- as.numeric(eps)
    P[[root]] <- (1 - e) * P[[root]] + e * et
  }
  for (sim in seq_len(as.integer(num_sim))) {
    node <- root; path <- list(); depth <- 0L; v <- 0
    repeat {
      if (!is.null(terminal) && terminal(ids[[node]])) { v <- V[node]; break }
      if (depth >= max_depth) { v <- V[node]; break }
      tot <- 0
      for (x in N[[node]]) tot <- tot + x
      rt <- if (tot > 0) sqrt(tot) else 0
      best <- 1L; bestscore <- NULL
      for (a in seq_along(P[[node]])) {
        q <- if (N[[node]][a] > 0) W[[node]][a] / N[[node]][a] else 0
        sc <- q + c_puct * P[[node]][a] * rt / (1 + N[[node]][a])
        if (is.null(bestscore) || sc > bestscore) { bestscore <- sc; best <- a }
      }
      path[[length(path) + 1L]] <- c(node, best)
      s2 <- if (!is.null(step)) step(ids[[node]], best - 1L) else NULL
      if (is.null(s2)) { v <- V[node]; break }
      idx <- find(s2)
      if (idx < 0L) { idx <- expand(s2); v <- V[idx]; break }
      node <- idx; depth <- depth + 1L
    }
    acc <- v
    if (length(path) > 0L) for (i in seq(length(path), 1L)) {
      if (alternate) acc <- -acc
      nd <- path[[i]][1]; a <- path[[i]][2]
      N[[nd]][a] <- N[[nd]][a] + 1
      W[[nd]][a] <- W[[nd]][a] + acc
    }
  }
  tot <- 0
  for (x in N[[root]]) tot <- tot + x
  pi_ <- if (tot > 0) N[[root]] / tot else rep(0, length(N[[root]]))
  q <- numeric(length(N[[root]]))
  for (a in seq_along(q)) q[a] <- if (N[[root]][a] > 0) W[[root]][a] / N[[root]][a] else 0
  best <- 1L
  if (length(pi_) > 1L) for (a in seq(2L, length(pi_))) if (pi_[a] > pi_[best]) best <- a
  wsum <- 0
  for (x in W[[root]]) wsum <- wsum + x
  list(estimate = as.numeric(best - 1L), action = best - 1L, pi = pi_,
       n = N[[root]], q = q, p = P[[root]],
       value = if (tot > 0) wsum / tot else 0, n_nodes = length(ids),
       method = "AlphaZero MCTS with a neural prior (PUCT selection)")
}
