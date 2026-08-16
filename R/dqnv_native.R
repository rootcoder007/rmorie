# morie.fn -- function file (rootcoder007/morie)
# Deep Q-networks: two devices that make the divergence go away.
#
# Q-learning with a non-linear function approximator was known to be
# unstable. The Nature paper isolates why and fixes it with two
# mechanisms, neither of which changes the learning rule itself.
#
# **Experience replay.** Transitions
# :math:`e_t = (s_t, a_t, r_t, s_{t+1})` are stored, and updates are
# computed on minibatches drawn **uniformly at random** from the pool.
# That breaks the correlation in the observation sequence -- consecutive
# frames are nearly identical, and training on them in order is training
# on a moving, highly dependent distribution. It also smooths over
# changes in the data distribution as the policy shifts, and lets each
# transition be used many times.
#
# **A target network held fixed.** The loss regresses
# :math:`Q(s,a;\theta_i)` onto
# :math:`r + \gamma\max_{a'}Q(s',a';\theta_i^-)`, where
# :math:`\theta^-` is a *separate* copy updated only every :math:`C`
# steps. Without it the target moves with every update -- the network
# chases its own output, and an increase in :math:`Q(s,a)` immediately
# raises the target for the neighbouring state, which is the feedback
# loop that diverges.
#
# **One learning rule, unchanged.** The update is ordinary Q-learning;
# what changed is *what data it sees* and *what it regresses onto*. The
# anchor exploits that: on a small tabular MDP the fixed point is known
# in closed form, so convergence can be checked against the true
# :math:`Q^*` rather than against another run.
#
# **A detail that is not incidental**: rewards are clipped to
# :math:`[-1,1]` so one learning rate works across games with wildly
# different score scales -- at the cost of making the agent indifferent
# between rewards of different magnitude.
#
# References
# ----------
# Mnih, V., Kavukcuoglu, K., Silver, D., Rusu, A. A., Veness, J.,
# Bellemare, M. G., Graves, A., Riedmiller, M., Fidjeland, A. K.,
# Ostrovski, G., Petersen, S., Beattie, C., Sadik, A., Antonoglou, I.,
# King, H., Kumaran, D., Wierstra, D., Legg, S. & Hassabis, D. (2015)
# "Human-level control through deep reinforcement learning", *Nature*
# 518(7540), 529-533, doi:10.1038/nature14236. The two key ideas:
# experience replay, which randomises over the data to remove
# correlations in the observation sequence and smooth over changes in
# the data distribution; and an iterative update towards target values
# that are only periodically updated, with theta^- held fixed between
# updates and refreshed every C steps. The Methods section gives the
# reward clipping to [-1, 1].
#
# Watkins, C. J. C. H. & Dayan, P. (1992) "Q-learning", *Machine
# Learning* 8, 279-292, doi:10.1007/BF00992698. The learning rule
# itself.
#
# Lin, L.-J. (1992) "Self-improving reactive agents based on
# reinforcement learning, planning and teaching", *Machine Learning* 8,
# 293-321, doi:10.1007/BF00992699. Experience replay.

.dqnv_eps <- 1e-12

#' .dqnv_clip_reward
#'
#' A step of the dqnv_native implementation. Called by \code{morie_dqnv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param lo Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{-1}.
#' @param hi Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.dqnv_clip_reward <- function(r, lo = -1.0, hi = 1.0) {
  max(as.numeric(lo), min(as.numeric(hi), as.numeric(r)))
}

#' .dqnv_td_target
#'
#' A step of the dqnv_native implementation. Called by \code{morie_dqnv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param s2 Coerced to integer by the body, with \code{as.integer}.
#' @param Q_target A vector; indexed elementwise.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.99}.
#' @param done Coerced to logical by the body, with \code{as.logical}. Defaults to \code{FALSE}.
#' @return A numeric value.
#' @export
.dqnv_td_target <- function(r, s2, Q_target, gamma = 0.99, done = FALSE) {
  if (as.logical(done)) {
    return(as.numeric(r))
  }
  row <- Q_target[[as.integer(s2) + 1L]]
  as.numeric(r) + as.numeric(gamma) * max(row)
}

#' .dqnv_bellman_residual
#'
#' A step of the dqnv_native implementation. Called by \code{morie_dqnv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Q A vector; its length is taken and its elements indexed.
#' @param P A vector; indexed elementwise.
#' @param R A vector; indexed elementwise.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.99}.
#' @return The value of \code{worst}, as built in the body.
#' @export
.dqnv_bellman_residual <- function(Q, P, R, gamma = 0.99) {
  nS <- length(Q)
  nA <- length(Q[[1L]])
  worst <- 0.0
  for (s_idx in seq_len(nS) - 1L) {
    for (a_idx in seq_len(nA) - 1L) {
      r_val <- R[[s_idx + 1L]][[a_idx + 1L]]
      p_row <- P[[s_idx + 1L]][[a_idx + 1L]]
      acc <- 0.0
      for (s2_idx in seq_len(nS) - 1L) {
        q_row <- Q[[s2_idx + 1L]]
        max_q <- max(q_row)
        acc <- acc + p_row[[s2_idx + 1L]] * max_q
      }
      t_val <- r_val + as.numeric(gamma) * acc
      diff <- Q[[s_idx + 1L]][[a_idx + 1L]] - t_val
      worst <- max(worst, abs(diff))
    }
  }
  worst
}

#' .dqnv_replay_buffer_new
#'
#' A step of the dqnv_native implementation. Called by \code{morie_dqnv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param capacity Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{capacity}, \code{data}.
#' @export
.dqnv_replay_buffer_new <- function(capacity) {
  capacity <- as.integer(capacity)
  if (capacity < 1L) stop("dqnv: the capacity must be at least 1")
  list(capacity = capacity, data = list())
}

#' .dqnv_replay_buffer_add
#'
#' A step of the dqnv_native implementation. Called by \code{morie_dqnv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param buf A list; the body reads \code{$capacity}, \code{$data} from it.
#' @param s Carried through into a list the body builds.
#' @param a Coerced to integer by the body, with \code{as.integer}.
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param s2 Coerced to integer by the body, with \code{as.integer}.
#' @param done Coerced to logical by the body, with \code{as.logical}. Defaults to \code{FALSE}.
#' @return The value of \code{buf}, as built in the body.
#' @export
.dqnv_replay_buffer_add <- function(buf, s, a, r, s2, done = FALSE) {
  buf$data[[length(buf$data) + 1L]] <- list(
    s, as.integer(a), as.numeric(r), as.integer(s2), as.logical(done)
  )
  if (length(buf$data) > buf$capacity) {
    buf$data <- buf$data[-1L]
  }
  buf
}

#' .dqnv_replay_buffer_sample
#'
#' A step of the dqnv_native implementation. Called by \code{morie_dqnv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param buf A list; the body reads \code{$data} from it.
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param rng_state Passed to \code{.ghc_unif}.
#' @return A list with \code{state}, \code{samples}.
#' @export
.dqnv_replay_buffer_sample <- function(buf, n, rng_state) {
  if (length(buf$data) == 0L) stop("dqnv: the buffer is empty")
  m <- min(as.integer(n), length(buf$data))
  results <- vector("list", m)
  n_data <- length(buf$data)
  for (i in seq_len(m)) {
    res <- .ghc_unif(rng_state, 1L)
    rng_state <- res$s
    u_val <- res$v[1L]
    idx <- (as.integer(u_val * n_data)) %% n_data
    results[[i]] <- buf$data[[idx + 1L]]
  }
  list(state = rng_state, samples = results)
}

#' .dqnv_replay_buffer_len
#'
#' A step of the dqnv_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param buf A list; the body reads \code{$data} from it.
#' @return The value of \code{length}.
#' @export
.dqnv_replay_buffer_len <- function(buf) {
  length(buf$data)
}

#' morie_dqnv
#'
#' A step of the dqnv_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param P A vector; indexed elementwise.
#' @param R A vector; indexed elementwise.
#' @param n_states Coerced to integer by the body, with \code{as.integer}.
#' @param n_actions Coerced to integer by the body, with \code{as.integer}.
#' @param gamma Passed to \code{.dqnv_td_target}. Defaults to \code{0.99}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.
#' @param steps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20000}.
#' @param C Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param buffer_size Passed to \code{.dqnv_replay_buffer_new}. Defaults to \code{1000}.
#' @param batch Passed to \code{.dqnv_replay_buffer_sample}. Defaults to \code{16}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param use_replay A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param use_target A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{Q}, \code{residual_history}, \code{final_residual}, \code{greedy_policy}, \code{used_replay}, \code{used_target_network}, \code{C}, \code{method}.
#' @export
morie_dqnv <- function(P, R, n_states, n_actions, gamma = 0.99, alpha = 0.1,
                      steps = 20000, C = 100, buffer_size = 1000, batch = 16,
                      seed = 0, use_replay = TRUE, use_target = TRUE) {
  nS <- as.integer(n_states)
  nA <- as.integer(n_actions)
  if (nS < 1L || nA < 1L) stop("dqnv: need at least one state and action")

  rng_state <- .ghc_rng(seed)

  Q <- lapply(seq_len(nS), function(x) rep(0.0, nA))
  Qt <- lapply(Q, function(row) as.numeric(row))

  buf <- .dqnv_replay_buffer_new(buffer_size)
  s <- 0L

  interval <- max(1L, as.integer(steps) %/% 20L)
  hist <- list()

  C_int <- as.integer(C)

  for (t in seq_len(as.integer(steps)) - 1L) {
    res <- .ghc_unif(rng_state, 1L)
    rng_state <- res$s
    a <- (as.integer(res$v[1L] * nA)) %% nA

    res <- .ghc_unif(rng_state, 1L)
    rng_state <- res$s
    u <- res$v[1L]
    acc <- 0.0
    s2 <- nS - 1L

    p_row <- P[[s + 1L]][[a + 1L]]
    for (j in seq_len(nS) - 1L) {
      acc <- acc + p_row[[j + 1L]]
      if (u <= acc) {
        s2 <- j
        break
      }
    }

    r <- .dqnv_clip_reward(R[[s + 1L]][[a + 1L]])
    buf <- .dqnv_replay_buffer_add(buf, s, a, r, s2, FALSE)

    if (use_replay) {
      sample_result <- .dqnv_replay_buffer_sample(buf, batch, rng_state)
      rng_state <- sample_result$state
      batchset <- sample_result$samples
    } else {
      batchset <- list(list(s, a, r, s2, FALSE))
    }

    for (item in batchset) {
      bs <- item[[1L]]
      ba <- item[[2L]]
      br <- item[[3L]]
      bs2 <- item[[4L]]
      bd <- item[[5L]]

      if (use_target) {
        y <- .dqnv_td_target(br, bs2, Qt, gamma, bd)
      } else {
        y <- .dqnv_td_target(br, bs2, Q, gamma, bd)
      }
      Q[[bs + 1L]][ba + 1L] <- Q[[bs + 1L]][ba + 1L] +
        as.numeric(alpha) * (y - Q[[bs + 1L]][ba + 1L])
    }

    if (use_target && ((t + 1L) %% C_int) == 0L) {
      Qt <- lapply(Q, function(row) as.numeric(row))
    }

    if (((t + 1L) %% interval) == 0L) {
      hist[[length(hist) + 1L]] <- .dqnv_bellman_residual(Q, P, R, gamma)
    }

    s <- s2
  }

  greedy_policy <- sapply(seq_len(nS), function(s_idx) {
    which.max(Q[[s_idx]]) - 1L
  })

  final_residual <- if (length(hist) > 0L) hist[[length(hist)]] else NaN

  list(
    estimate = Q,
    Q = Q,
    residual_history = hist,
    final_residual = final_residual,
    greedy_policy = greedy_policy,
    used_replay = as.logical(use_replay),
    used_target_network = as.logical(use_target),
    C = C_int,
    method = "Q-learning with experience replay and a frozen target network; Mnih et al. (2015)"
  )
}

#' .dqnv_cheatsheet
#'
#' A step of the dqnv_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.dqnv_cheatsheet <- function() {
  paste("dqnv: the LEARNING RULE is ordinary Q-learning; what",
        "changed is the data and the target. EXPERIENCE REPLAY",
        "samples transitions UNIFORMLY from a finite buffer,",
        "breaking the correlation between consecutive frames and",
        "smoothing distribution shift. The TARGET NETWORK is a",
        "frozen copy refreshed every C steps -- without it the",
        "network chases its own output, since raising Q(s,a)",
        "immediately raises the target at the neighbouring state.",
        "Rewards clipped to [-1,1] so one learning rate spans",
        "games, at the cost of indifference to magnitude.",
        sep = " ")
}

# compact alias per ledger/NAMING.md
morie_dqnv_deepqnetwork <- morie_dqnv

# public names resolved by fn/_lazy_map.json
morie_dqnv_deep_q_network <- morie_dqnv
