# SPDX-License-Identifier: AGPL-3.0-or-later
#' Goal-conditioned value functions
#'
#' Schaul, Horgan, Gregor and Silver (2015), Universal value function
#' approximators, ICML 37, 1312-1320, generalise a value function to
#' V(s, g) with a goal-dependent pseudo-reward; the canonical choice, used
#' in that paper and in Andrychowicz et al. (2017), Hindsight experience
#' replay (arXiv:1707.01495), is the sparse indicator r_g(s) = 0 if s == g
#' and -1 otherwise, so V(s, g) is the negated expected number of steps to
#' reach g.  The ICML version is free but was not retrievable here; the
#' definition is quoted in its standard published form and is reproduced
#' identically in both papers.
#'
#' On a deterministic transition list the values are exact, computed by a
#' backward breadth-first sweep from each goal -- value iteration
#' specialised to unit costs, so no sampling and no generator.
#'
#' @param env deterministic transitions, one row per (s, a, s').
#' @param policy ignored; the values returned are the optimal ones.
#' @param goal_dist goals, or (goal, weight) pairs; default every state.
#' @param n_states number of states; inferred when absent.
#' @param gamma discount; 1 gives negated shortest-path length.
#' @param step_cost per-step pseudo-reward.
#' @return list: estimate, v, expected_value, reachable, n_states, method.
#' @keywords internal
#' @examples
#' Goalcond(matrix(c(0, 0, 1, 1, 0, 2), 2, 3, byrow = TRUE))$expected_value
#' @export
Goalcond <- function(env, policy = NULL, goal_dist = NULL, n_states = NULL,
                     gamma = 1, step_cost = -1) {
  rows <- .s03mat(env)
  nr <- nrow(rows)
  ns <- if (!is.null(n_states)) as.integer(n_states) else
    if (nr) as.integer(max(pmax(rows[, 1], rows[, 3])) + 1) else 0L
  pred <- vector("list", ns)
  for (i in seq_len(ns)) pred[[i]] <- integer(0)
  for (i in seq_len(nr)) {
    s <- as.integer(rows[i, 1]) + 1L; s2 <- as.integer(rows[i, 3]) + 1L
    pred[[s2]] <- c(pred[[s2]], s)
  }
  gd <- if (!is.null(goal_dist)) .s03mat(goal_dist) else
    cbind(seq_len(ns) - 1, rep(1, ns))
  if (ncol(gd) == 1L) gd <- cbind(gd, rep(1, nrow(gd)))
  goals <- as.integer(gd[, 1]); wts <- gd[, 2]
  wtot <- 0
  for (x in wts) wtot <- wtot + x
  ng <- length(goals)
  V <- matrix(-Inf, ns, ng)
  reach <- 0L
  for (gi in seq_len(ng)) {
    g <- goals[gi] + 1L
    dist <- rep(-1L, ns)
    dist[g] <- 0L
    frontier <- c(g)
    while (length(frontier) > 0L) {
      nxt <- integer(0)
      for (u in frontier) {
        for (p in pred[[u]]) if (dist[p] < 0L) { dist[p] <- dist[u] + 1L; nxt <- c(nxt, p) }
      }
      frontier <- nxt
    }
    for (s in seq_len(ns)) {
      if (dist[s] < 0L) {
        V[s, gi] <- -Inf
      } else {
        d <- dist[s]
        if (as.numeric(gamma) == 1) {
          V[s, gi] <- as.numeric(step_cost) * d
        } else {
          acc <- 0
          if (d > 0L) for (j in seq_len(d) - 1L) acc <- acc + (as.numeric(gamma)^j) * as.numeric(step_cost)
          V[s, gi] <- acc
        }
        reach <- reach + 1L
      }
    }
  }
  ev <- numeric(ns)
  for (s in seq_len(ns)) {
    acc <- 0
    for (gi in seq_len(ng)) acc <- acc + wts[gi] * (if (is.finite(V[s, gi])) V[s, gi] else 0)
    ev[s] <- if (wtot > 0) acc / wtot else NaN
  }
  list(estimate = if (ns) ev[1] else NaN, v = V, expected_value = ev,
       reachable = if (ns && ng) reach / (ns * ng) else NaN, n_states = ns,
       method = "Goal-conditioned V*(s, g) with the sparse pseudo-reward (UVFA)")
}
