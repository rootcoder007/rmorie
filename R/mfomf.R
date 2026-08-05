# SPDX-License-Identifier: AGPL-3.0-or-later
#' Model-based RL: fit a tabular model, then plan in it
#'
#' Sutton (1991), Dyna, an integrated architecture for learning, planning,
#' and reacting, SIGART Bulletin 2(4), 160-163: the same experience is
#' used twice, once to learn a model and once to plan with it.  The model
#' is the maximum-likelihood tabular one, phat(s'|s,a) = N(s,a,s')/N(s,a)
#' and rhat(s,a) = sum of rewards / N(s,a); the planner is value iteration
#' (Bellman 1957, Dynamic Programming), V(s) <- max_a \[rhat(s,a) + gamma
#' sum_s' phat(s'|s,a) V(s')].  Neither source was available here as a
#' full text; both equations are quoted in their standard published form
#' and reproduced identically in Sutton and Barto (2018) sections 8.1-8.2
#' and 4.4 (FETCHED).  Unvisited (s, a) pairs are excluded from the
#' maximisation rather than assigned an invented value.
#'
#' @param env transitions, one row per experience: (s, a, r, s').
#' @param model ignored; present for signature stability.
#' @param planner only "vi" is implemented.
#' @param n_states,n_actions tabular sizes; inferred when absent.
#' @param gamma discount.
#' @param tol sweep tolerance.
#' @param max_iter sweep cap.
#' @return list: estimate, v, policy, sweeps, n_states, n_actions, method.
#' @keywords internal
#' @examples
#' Modelrl(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), 2, 4, byrow = TRUE))$v
#' @export
Modelrl <- function(env, model = NULL, planner = "vi", n_states = NULL,
                    n_actions = NULL, gamma = 0.95, tol = 1e-12,
                    max_iter = 1000) {
  if (!identical(planner, "vi")) stop("only planner='vi' (value iteration) is implemented")
  rows <- .s03mat(env)
  nr <- nrow(rows)
  ns <- if (!is.null(n_states)) as.integer(n_states) else
    if (nr) as.integer(max(pmax(rows[, 1], rows[, 4])) + 1) else 0L
  na <- if (!is.null(n_actions)) as.integer(n_actions) else
    if (nr) as.integer(max(rows[, 2]) + 1) else 0L
  cnt <- matrix(0, ns, na); rsum <- matrix(0, ns, na)
  trans <- array(0, c(ns, na, ns))
  for (i in seq_len(nr)) {
    s <- as.integer(rows[i, 1]) + 1L; a <- as.integer(rows[i, 2]) + 1L
    rw <- rows[i, 3]; s2 <- as.integer(rows[i, 4]) + 1L
    cnt[s, a] <- cnt[s, a] + 1
    rsum[s, a] <- rsum[s, a] + rw
    trans[s, a, s2] <- trans[s, a, s2] + 1
  }
  V <- numeric(ns); sweeps <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    sweeps <- sweeps + 1L
    delta <- 0
    for (s in seq_len(ns)) {
      best <- NULL
      for (a in seq_len(na)) {
        if (cnt[s, a] <= 0) next
        q <- rsum[s, a] / cnt[s, a]
        acc <- 0
        for (s2 in seq_len(ns)) if (trans[s, a, s2] > 0) acc <- acc + (trans[s, a, s2] / cnt[s, a]) * V[s2]
        q <- q + as.numeric(gamma) * acc
        if (is.null(best) || q > best) best <- q
      }
      nv <- if (!is.null(best)) best else V[s]
      d <- abs(nv - V[s])
      if (d > delta) delta <- d
      V[s] <- nv
    }
    if (delta <= as.numeric(tol)) break
  }
  pol <- integer(ns)
  for (s in seq_len(ns)) {
    best <- NULL; ba <- -1L
    for (a in seq_len(na)) {
      if (cnt[s, a] <= 0) next
      q <- rsum[s, a] / cnt[s, a]
      acc <- 0
      for (s2 in seq_len(ns)) if (trans[s, a, s2] > 0) acc <- acc + (trans[s, a, s2] / cnt[s, a]) * V[s2]
      q <- q + as.numeric(gamma) * acc
      if (is.null(best) || q > best) { best <- q; ba <- a - 1L }
    }
    pol[s] <- ba
  }
  list(estimate = if (ns) V[1] else NaN, v = V, policy = pol, sweeps = sweeps,
       n_states = ns, n_actions = na,
       method = "Maximum-likelihood tabular model plus value iteration (Dyna)")
}
