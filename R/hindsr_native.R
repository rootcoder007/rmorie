# Hindsight Experience Replay: goal relabelling of stored transitions.
#
# Sources:
#   Andrychowicz, M., Wolski, F., Ray, A., Schneider, J., Fong, R.,
#   Welinder, P., McGrew, B., Tobin, J., Abbeel, P., & Zaremba, W.
#   (2017) "Hindsight Experience Replay", arXiv:1707.01495.

._STRATEGIES <- c("future", "final", "episode", "random")

._as_states <- function(seq, name) {
  out <- lapply(seq, function(s) as.numeric(s))
  if (length(out) == 0L) {
    stop(sprintf("hindsr: %s must be non-empty", name))
  }
  out
}

._sparse_reward <- function(s, a, s_next, g, tol) {
  if (length(s_next) != length(g)) {
    stop(sprintf("hindsr: goal has length %d but state has %d; pass a state_to_goal mapping", length(g), length(s_next)))
  }
  for (i in seq_along(g)) {
    if (abs(s_next[i] - g[i]) > tol) return(-1.0)
  }
  0.0
}

._sample_goals <- function(strategy, episode, t, T, k, m, pool, e) {
  if (strategy == "final") {
    return(list(m(episode[[T + 1L]])))
  }
  if (strategy == "future") {
    lo <- t + 1L
    hi <- T + 1L
    if (hi < lo) return(list())
    idx <- lo + floor(.ghc_unif(e, k) * (hi - lo + 1L))
    return(lapply(idx, function(i) m(episode[[i]])))
  }
  if (strategy == "episode") {
    idx <- floor(.ghc_unif(e, k) * (T + 1L)) + 1L
    return(lapply(idx, function(i) m(episode[[i]])))
  }
  n <- length(pool)
  idx <- floor(.ghc_unif(e, k) * n) + 1L
  lapply(idx, function(i) m(pool[[i]]))
}

hindsr <- function(episodes, actions = NULL, goals = NULL,
                   strategy = "future", k = 4L, tol = 1e-6,
                   reward_fn = NULL, state_to_goal = NULL, seed = 0L,
                   history = NULL) {
  if (!(strategy %in% ._STRATEGIES)) {
    stop(sprintf("hindsr: strategy must be one of %s, got %r",
                 paste(sQuote(._STRATEGIES), collapse = ", "), strategy))
  }
  k <- as.integer(k)
  if (k < 1L) stop("hindsr: k must be >= 1")
  tol <- as.numeric(tol)
  m <- if (is.null(state_to_goal)) function(s) as.numeric(s) else
    state_to_goal
  if (!is.function(m)) {
    stop("hindsr: state_to_goal must be callable")
  }
  rf <- if (is.null(reward_fn)) {
    function(s, a, s_next, g) ._sparse_reward(s, a, s_next, g, tol)
  } else {
    if (!is.function(reward_fn)) {
      stop("hindsr: reward_fn must be callable")
    }
    reward_fn
  }

  eps <- ._as_states(episodes, "episode")
  for (i in seq_along(eps)) {
    if (length(eps[[i]]) < 2L) {
      stop(sprintf("hindsr: episode %d has %d states; need at least s_0 and s_1", i - 1L, length(eps[[i]])))
    }
  }
  n_ep <- length(eps)
  if (is.null(actions)) {
    acts <- lapply(eps, function(e) seq_len(length(e) - 1L) - 1L)
  } else {
    acts <- lapply(actions, function(a) as.numeric(a))
    if (length(acts) != n_ep) {
      stop(sprintf("hindsr: got %d action sequences for %d episodes",
                   length(acts), n_ep))
    }
    for (i in 1:n_ep) {
      if (length(acts[[i]]) != length(eps[[i]]) - 1L) {
        stop(sprintf("hindsr: episode %d has %d states but %d actions",
                     i - 1L, length(eps[[i]]), length(acts[[i]])))
      }
    }
  }
  if (is.null(goals)) {
    gs <- lapply(eps, function(e) m(e[[length(e)]]))
  } else {
    gs <- lapply(goals, function(g) as.numeric(g))
    if (length(gs) != n_ep) {
      stop(sprintf("hindsr: got %d goals for %d episodes",
                   length(gs), n_ep))
    }
  }
  if (is.null(history)) {
    pool <- do.call(c, eps)
  } else {
    pool <- ._as_states(history, "history")
  }

  e <- .ghc_rng(seed)
  buf <- list()
  n_relabelled <- 0L
  for (i in 1:n_ep) {
    ep <- eps[[i]]
    T <- length(ep) - 1L
    for (t in 1:T) {
      s <- ep[[t]]
      a <- acts[[i]][t]
      s1 <- ep[[t + 1L]]
      buf[[length(buf) + 1L]] <- list(s = s, a = a,
                                      reward = as.numeric(rf(s, a, s1, gs[[i]])),
                                      next_state = s1, goal = gs[[i]],
                                      relabelled = FALSE)
      g2s <- ._sample_goals(strategy, ep, t, T, k, m, pool, e)
      for (g2 in g2s) {
        buf[[length(buf) + 1L]] <- list(s = s, a = a,
                                        reward = as.numeric(rf(s, a, s1, g2)),
                                        next_state = s1, goal = g2,
                                        relabelled = TRUE)
        n_relabelled <- n_relabelled + 1L
      }
    }
  }
  rewards <- vapply(buf, function(tr) tr$reward, numeric(1))
  n_success <- sum(rewards > -1.0 + 1e-12)
  list(estimate = length(buf), transitions = buf,
       n_transitions = length(buf),
       n_original = length(buf) - n_relabelled,
       n_relabelled = n_relabelled,
       rewards = rewards,
       success_rate = if (length(buf) > 0L) n_success / length(buf) else 0.0,
       strategy = strategy, k = k, n_episodes = n_ep,
       method = "HER (Andrychowicz et al. 2017, Algorithm 1)")
}

her <- hindsr

morie_hindsr <- hindsr

cheatsheet <- function() {
  paste("hindsr: HER (Andrychowicz 2017 Alg. 1). Store each transition with the original goal, then again for each g' in S(episode) with the reward RECOMPUTED under g'. S in {final, future (k, best), episode, random}; r(s,a,g) = -[f_g(s') = 0].")
}
