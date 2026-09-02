# Deep meta-reinforcement learning: an RL algorithm that learns an
# RL algorithm.  Source: Wang, J. X. et al. (2016), "Learning to
# reinforcement learn", arXiv:1611.05763.
#
# Native implementation mirroring Python morie.fn.mtdrl exactly: the
# same history-conditioned evaluation loop, the same per-episode
# recurrent-state reset, the same dependent-arm family, and the same
# reference TabularHistoryAgent.

#' A distribution of bandit tasks
#'
#' @param n_arms Number of arms.
#' @param n_tasks Number of tasks.
#' @param seed Seed for the shared generator.
#' @param structure "independent" or "paired".
#' @return A list of per-task arm-probability vectors.
#' @export
morie_mtdrl_bandit_tasks <- function(n_arms = 2, n_tasks = 100,
                                      seed = 0,
                                      structure = "independent") {
  if (!(structure %in% c("independent", "paired")))
    stop(paste0("mtdrl: structure must be 'independent' or 'paired', ",
                "got ", structure))
  n_arms <- as.integer(n_arms)
  if (n_arms < 2L) stop("mtdrl: need at least 2 arms")
  if (structure == "paired" && n_arms != 2L)
    stop("mtdrl: the paired family is defined for 2 arms")
  e <- .ghc_rng(seed)
  tasks <- list()
  for (k in seq_len(as.integer(n_tasks))) {
    if (structure == "paired") {
      p <- .ghc_unif(e, 1L)
      tasks[[length(tasks) + 1L]] <- c(p, 1 - p)
    } else {
      tasks[[length(tasks) + 1L]] <- .ghc_unif(e, n_arms)
    }
  }
  tasks
}

#' History feature vector
#'
#' @param history List of (action, reward) pairs so far.
#' @param n_arms Number of arms.
#' @return Numeric feature vector of length n_arms + 2: one-hot
#'   previous action, previous reward, step index.
#' @export
morie_mtdrl_history_features <- function(history, n_arms) {
  feat <- rep(0, n_arms + 2L)
  if (length(history) > 0L) {
    a <- history[[length(history)]][[1]] + 1L
    r <- as.numeric(history[[length(history)]][[2]])
    feat[a] <- 1
    feat[n_arms + 1L] <- r
  }
  feat[n_arms + 2L] <- length(history)
  feat
}

#' A reference inner learner
#'
#' Per-episode counts and means; epsilon-greedy; weights never
#' updated across episodes.
#'
#' @field n_arms,epsilon,optimistic Configuration.
#' @field counts,means Within-episode statistics.
#' @export
#' @noRd
morie_mtdrl_TabularHistoryAgent <- setRefClass(
  "morie_mtdrl_TabularHistoryAgent",
  fields = list(
    n_arms = "integer",
    epsilon = "numeric",
    optimistic = "numeric",
    counts = "integer",
    means = "numeric"
  ),
  methods = list(
    initialize = function(n_arms = 2L, epsilon = 0.1, optimistic = 1) {
      n_arms <<- as.integer(n_arms)
      epsilon <<- as.numeric(epsilon)
      optimistic <<- as.numeric(optimistic)
      counts <<- rep(0L, n_arms)
      means <<- rep(optimistic, n_arms)
    },
    reset = function() {
      counts <<- rep(0L, n_arms)
      means <<- rep(optimistic, n_arms)
    },
    act = function(features, e) {
      if (.ghc_unif(e, 1L) < epsilon) {
        return(as.integer(.ghc_unif(e, 1L) * n_arms))
      }
      best <- max(means)
      cand <- which(means >= best - 1e-15) - 1L
      cand[as.integer(.ghc_unif(e, 1L) * length(cand)) + 1L]
    },
    observe = function(action, reward) {
      action <- as.integer(action) + 1L
      counts[action] <<- counts[action] + 1L
      n <- counts[action]
      means[action] <<- means[action] +
        (as.numeric(reward) - means[action]) / n
    }
  )
)

#' Run the meta-RL evaluation loop
#'
#' @param tasks List of per-task probability vectors.
#' @param agent A list with reset(), act(features, e), observe(a, r).
#' @param episode_length Steps per episode.
#' @param n_arms Optional, inferred from tasks.
#' @param seed Seed.
#' @param reset_between_episodes Apply the per-episode reset.
#' @return A list with mean_reward, total_reward, regret,
#'   reward_by_step, optimal_action_rate, episode_reward, n_episodes,
#'   episode_length, n_arms, method.
#' @export
morie_mtdrl <- function(tasks, agent, episode_length = 100,
                         n_arms = NULL, seed = 0,
                         reset_between_episodes = TRUE) {
  T <- lapply(tasks, function(t) as.numeric(t))
  if (length(T) == 0L) stop("mtdrl: tasks must be non-empty")
  k <- if (is.null(n_arms)) length(T[[1]]) else as.integer(n_arms)
  for (t in T)
    if (length(t) != k)
      stop(paste0("mtdrl: every task must have ", k, " arms"))
  L <- as.integer(episode_length)
  if (L < 1L) stop("mtdrl: episode_length must be >= 1")
  for (m in c("reset", "act", "observe"))
    if (!m %in% names(agent))
      stop(paste0("mtdrl: agent must provide ", m, "()"))
  e <- .ghc_rng(seed)
  total <- 0; regret <- 0
  by_step <- rep(0, L); opt_by_step <- rep(0, L)
  per_episode <- c()
  for (probs in T) {
    if (isTRUE(reset_between_episodes)) agent$reset()
    best_p <- max(probs)
    best_arms <- which(probs >= best_p - 1e-15) - 1L
    hist <- list()
    ep_reward <- 0
    for (t in seq_len(L)) {
      feats <- morie_mtdrl_history_features(hist, k)
      a <- as.integer(agent$act(feats, e))
      if (a < 0L || a >= k)
        stop(paste0("mtdrl: agent chose arm ", a,
                    " outside 0..", k - 1L))
      u <- .ghc_unif(e, 1L)
      r <- if (u < probs[a + 1L]) 1 else 0
      agent$observe(a, r)
      hist[[length(hist) + 1L]] <- list(a, r)
      ep_reward <- ep_reward + r
      total <- total + r
      regret <- regret + (best_p - probs[a + 1L])
      by_step[t] <- by_step[t] + r
      opt_by_step[t] <- opt_by_step[t] + if (a %in% best_arms) 1 else 0
    }
    per_episode <- c(per_episode, ep_reward)
  }
  n_ep <- length(T)
  list(estimate = total / (n_ep * L),
       mean_reward = total / (n_ep * L),
       total_reward = total,
       regret = regret,
       reward_by_step = by_step / n_ep,
       optimal_action_rate = opt_by_step / n_ep,
       episode_reward = per_episode,
       n_episodes = n_ep,
       episode_length = L,
       n_arms = k,
       method = "meta-RL evaluation loop (Wang et al. 2016 sec. 2)")
}

morie_mtdrl_meta_rl <- morie_mtdrl
morie_mtdrl_metarl <- morie_mtdrl
