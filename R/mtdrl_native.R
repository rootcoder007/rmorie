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
#' @examples
#' morie_mtdrl_bandit_tasks()
#' @keywords internal
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
#' @examples
#' morie_mtdrl_history_features(history = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)),
#'   n_arms = 5L)
#' @keywords internal
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
#' @keywords internal
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
  total <- 0
  regret <- 0
  by_step <- rep(0, L)
  opt_by_step <- rep(0, L)
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

# -- restored: morie-only definition kept through the rmorie sync --
#' mtdrl_bandit_tasks
#'
#' A step of the mtdrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_arms Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.
#' @param n_tasks Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param seed Passed to \code{set.seed}. Defaults to \code{0}.
#' @param structure One of \code{"independent"}, \code{"paired"}. Defaults to \code{"independent"}.
#' @return The value of \code{tasks}, as built in the body.
#' @export
mtdrl_bandit_tasks <- function(n_arms = 2, n_tasks = 100, seed = 0,
                               structure = "independent") {
  if (!(structure %in% c("independent", "paired"))) {
    stop(sprintf("mtdrl: structure must be 'independent' or 'paired', got %s", structure))
  }
  n_arms <- as.integer(n_arms)
  if (n_arms < 2L) stop("mtdrl: need at least 2 arms")
  if (structure == "paired" && n_arms != 2L) {
    stop("mtdrl: the paired family is defined for 2 arms")
  }
  .rmorie_local_seed(seed)
  tasks <- list()
  for (i in seq_len(as.integer(n_tasks))) {
    if (structure == "paired") {
      p <- runif(1)
      tasks[[length(tasks) + 1L]] <- c(p, 1 - p)
    } else {
      tasks[[length(tasks) + 1L]] <- runif(n_arms)
    }
  }
  tasks
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mtdrl_cheatsheet
#'
#' A step of the mtdrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
mtdrl_cheatsheet <- function() {
  paste(paste0(
    "mtdrl: deep meta-RL (Wang 2016). Train with one RL algorithm",
    " so the RECURRENT DYNAMICS implement a second, learned one. ",
    "Policy conditions on the whole within-episode history H_t in",
    "cluding the previous ACTION and REWARD; the recurrent state ",
    "is RESET each episode, and after training the weights are fr",
    "ozen so all within-episode adaptation is in the activations.",
    " bandit_tasks(structure='paired') is the dependent-arm famil",
    "y whose structure an adapted inner algorithm can exploit."
  ))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mtdrl_history_features
#'
#' A step of the mtdrl_native implementation. Called by \code{mtdrl_run}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param history A vector; its length is taken and its elements indexed.
#' @param n_arms Numeric; combined arithmetically in the body.
#' @return The value of \code{feat}, as built in the body.
#' @export
mtdrl_history_features <- function(history, n_arms) {
  feat <- rep(0, n_arms + 2L)
  if (length(history) > 0L) {
    last <- history[[length(history)]]
    feat[last[[1]] + 1L] <- 1
    feat[n_arms + 1L] <- as.numeric(last[[2]])
  }
  feat[n_arms + 2L] <- length(history)
  feat
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mtdrl_run
#'
#' A step of the mtdrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tasks Iterated over elementwise, with \code{lapply}.
#' @param agent A list; the body reads \code{$act}, \code{$observe}, \code{$reset} from it.
#' @param episode_length Coerced to integer by the body, with \code{as.integer}. Defaults
#' to \code{100}.
#' @param n_arms Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @param seed Passed to \code{set.seed}. Defaults to \code{0}.
#' @param reset_between_episodes A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{mean_reward}, \code{total_reward},
#' \code{regret}, \code{reward_by_step}, \code{optimal_action_rate},
#' \code{episode_reward}, \code{n_episodes}, \code{episode_length}, \code{n_arms},
#' \code{method}.
#' @export
mtdrl_run <- function(tasks, agent, episode_length = 100, n_arms = NULL,
                      seed = 0, reset_between_episodes = TRUE) {
  T_ <- lapply(tasks, as.numeric)
  if (length(T_) == 0L) stop("mtdrl: tasks must be non-empty")
  k <- if (is.null(n_arms)) length(T_[[1]]) else as.integer(n_arms)
  for (t in T_) {
    if (length(t) != k) stop(sprintf("mtdrl: every task must have %d arms", k))
  }
  L <- as.integer(episode_length)
  if (L < 1L) stop("mtdrl: episode_length must be >= 1")
  for (m in c("reset", "act", "observe")) {
    if (!exists(m, envir = agent, inherits = FALSE)) {
      stop(sprintf("mtdrl: agent must provide %s()", m))
    }
  }
  .rmorie_local_seed(seed)
  rng <- function() runif(1)
  total <- 0
  regret <- 0
  by_step <- rep(0, L)
  opt_by_step <- rep(0, L)
  per_episode <- c()
  for (probs in T_) {
    if (reset_between_episodes) agent$reset()
    best_p <- max(probs)
    best_arms <- which(probs >= best_p)
    hist <- list()
    ep_reward <- 0
    for (t in seq_len(L)) {
      feats <- mtdrl_history_features(hist, k)
      a <- as.integer(agent$act(feats, rng))
      if (!(a >= 0 && a < k)) {
        stop(sprintf("mtdrl: agent chose arm %d outside 0..%d", a, k - 1L))
      }
      r <- if (runif(1) < probs[a + 1L]) 1 else 0
      agent$observe(a, r)
      hist[[length(hist) + 1L]] <- list(a, r)
      ep_reward <- ep_reward + r
      total <- total + r
      regret <- regret + (best_p - probs[a + 1L])
      by_step[t] <- by_step[t] + r
      opt_by_step[t] <- opt_by_step[t] + (if (a %in% best_arms) 1 else 0)
    }
    per_episode <- c(per_episode, ep_reward)
  }
  n_ep <- as.numeric(length(T_))
  list(estimate = total / (n_ep * L),
       mean_reward = total / (n_ep * L),
       total_reward = total,
       regret = regret,
       reward_by_step = by_step / n_ep,
       optimal_action_rate = opt_by_step / n_ep,
       episode_reward = per_episode,
       n_episodes = length(T_),
       episode_length = L,
       n_arms = k,
       method = "meta-RL evaluation loop (Wang et al. 2016 sec. 2)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mtdrl_TabularHistoryAgent
#'
#' A step of the mtdrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_arms Coerced to integer by the body, with \code{as.integer}.
#' @param epsilon Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.
#' @param optimistic Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return The value of \code{agent}, as built in the body.
#' @export
mtdrl_TabularHistoryAgent <- function(n_arms, epsilon = 0.1, optimistic = 1) {
  agent <- new.env(parent = emptyenv())
  agent$n_arms <- as.integer(n_arms)
  agent$epsilon <- as.numeric(epsilon)
  agent$optimistic <- as.numeric(optimistic)
  agent$counts <- rep(0L, agent$n_arms)
  agent$means <- rep(agent$optimistic, agent$n_arms)
  agent$reset <- function() {
    agent$counts <- rep(0L, agent$n_arms)
    agent$means <- rep(agent$optimistic, agent$n_arms)
  }
  agent$act <- function(features, rng) {
    if (rng() < agent$epsilon) {
      return(sample.int(agent$n_arms, 1L) - 1L)
    }
    best <- max(agent$means)
    cand <- which(agent$means >= best)
    cand[as.integer(rng() * length(cand)) + 1L] - 1L
  }
  agent$observe <- function(action, reward) {
    a <- as.integer(action) + 1L
    agent$counts[a] <- agent$counts[a] + 1L
    n <- agent$counts[a]
    agent$means[a] <- agent$means[a] + (as.numeric(reward) - agent$means[a]) / n
  }
  agent
}
