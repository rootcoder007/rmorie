# Anchors for deep Q-networks (Mnih et al. 2015).
#
# The Bellman residual gives an exact target: it is zero at the optimal Q,
# and adding a constant c to every entry of Q leaves a residual of exactly
# c(1 - gamma), since the target rises by gamma c. Value iteration
# supplies Q* independently, so the whole learner can be held to it.
#
# Experience replay is the paper's central mechanism and use_replay
# defaults to TRUE. Three calls in this file assumed .ghc_unif returned a
# list with $s and $v when it returns the draws and advances its
# environment in place, so morie_dqnv raised "$ operator is invalid for
# atomic vectors" on its own defaults and could not run at all.

# state-major nested lists, the shapes this module takes
mdp <- function() {
  S <- 4L; A <- 2L
  list(
    S = S, A = A, gamma = 0.9,
    # action 1 is best in states 0 and 1, action 2 in states 2 and 3
    R = list(list(1, 0), list(1, 0), list(0, 1), list(0, 1)),
    P = lapply(seq_len(S), function(s) list(c(0.7, 0.1, 0.1, 0.1),
                                            c(0.1, 0.1, 0.1, 0.7))))
}

q_star <- function(m) {
  Pmat <- lapply(seq_len(m$A), function(a)
    do.call(rbind, lapply(seq_len(m$S), function(s) m$P[[s]][[a]])))
  Rmat <- do.call(rbind, lapply(m$R, unlist))
  Q <- matrix(0, m$S, m$A)
  for (it in seq_len(20000)) {
    V <- apply(Q, 1, max)
    Qn <- Rmat
    for (a in seq_len(m$A)) {
      Qn[, a] <- Rmat[, a] + m$gamma * as.numeric(Pmat[[a]] %*% V)
    }
    if (max(abs(Qn - Q)) < 1e-15) return(Qn)
    Q <- Qn
  }
  Q
}

as_list_Q <- function(M) lapply(seq_len(nrow(M)), function(s) as.numeric(M[s, ]))

test_that("reward clipping is the documented scalar clamp", {
  expect_equal(.dqnv_clip_reward(7), 1)
  expect_equal(.dqnv_clip_reward(-5), -1)
  expect_equal(.dqnv_clip_reward(0.5), 0.5)
  expect_equal(.dqnv_clip_reward(0), 0)
  # the bounds are arguments
  expect_equal(.dqnv_clip_reward(7, lo = -10, hi = 10), 7)
  expect_equal(.dqnv_clip_reward(7, lo = 0, hi = 3), 3)
  # exactly at the bound, nothing moves
  expect_equal(.dqnv_clip_reward(1), 1)
  expect_equal(.dqnv_clip_reward(-1), -1)
})

test_that("the TD target reads the next state 0-based", {
  Qt <- list(c(1, 5), c(2, 3))
  expect_equal(.dqnv_td_target(2, 0, Qt, 0.9), 2 + 0.9 * 5, tolerance = 1e-12)
  expect_equal(.dqnv_td_target(2, 1, Qt, 0.9), 2 + 0.9 * 3, tolerance = 1e-12)
  # a terminal transition has no future
  expect_equal(.dqnv_td_target(2, 0, Qt, 0.9, done = TRUE), 2)
  expect_equal(.dqnv_td_target(-3, 1, Qt, 0.99, done = TRUE), -3)
  # gamma of zero is the same as terminal
  expect_equal(.dqnv_td_target(2, 0, Qt, 0), 2, tolerance = 1e-12)
})

test_that("the Bellman residual is zero at the optimal Q", {
  m <- mdp()
  Q <- q_star(m)
  expect_lt(.dqnv_bellman_residual(as_list_Q(Q), m$P, m$R, m$gamma), 1e-12)
  # and shifting every entry by c leaves exactly c(1 - gamma), because the
  # target rises by gamma c
  for (c_ in c(1, 2.5, -3)) {
    shifted <- as_list_Q(Q + c_)
    expect_equal(.dqnv_bellman_residual(shifted, m$P, m$R, m$gamma),
                 abs(c_) * (1 - m$gamma), tolerance = 1e-9)
  }
  # it is a sup norm, so it reports the worst state-action pair
  bad <- Q; bad[2, 1] <- bad[2, 1] + 5
  expect_gt(.dqnv_bellman_residual(as_list_Q(bad), m$P, m$R, m$gamma), 1)
})

test_that("the replay buffer is a ring of its capacity", {
  buf <- .dqnv_replay_buffer_new(3)
  expect_identical(as.integer(.dqnv_replay_buffer_len(buf)), 0L)
  for (k in 1:2) buf <- .dqnv_replay_buffer_add(buf, k, 1, k, k + 1, FALSE)
  expect_identical(as.integer(.dqnv_replay_buffer_len(buf)), 2L)
  for (k in 3:5) buf <- .dqnv_replay_buffer_add(buf, k, 1, k, k + 1, FALSE)
  # it never grows past its capacity
  expect_identical(as.integer(.dqnv_replay_buffer_len(buf)), 3L)
  # and it keeps the most recent transitions, dropping the oldest
  kept <- vapply(buf$data, function(tr) tr[[1]], numeric(1))
  expect_setequal(kept, c(3, 4, 5))
})

test_that("sampling the buffer returns transitions it holds", {
  # this raised on every call: the draw was read as res$v with the state
  # taken from res$s, neither of which .ghc_unif returns
  buf <- .dqnv_replay_buffer_new(5)
  for (k in 1:5) buf <- .dqnv_replay_buffer_add(buf, k, k %% 2L, k / 10, k + 1, FALSE)
  sm <- .dqnv_replay_buffer_sample(buf, 3, .ghc_rng(1L))
  expect_named(sm, c("state", "samples"), ignore.order = TRUE)
  expect_length(sm$samples, 3L)
  stored <- vapply(buf$data, function(tr) tr[[1]], numeric(1))
  for (tr in sm$samples) {
    expect_length(tr, 5L)
    # every sampled transition is one that was put in
    expect_true(tr[[1]] %in% stored)
  }
  # asking for more than it holds gives what it holds
  all_of <- .dqnv_replay_buffer_sample(buf, 50, .ghc_rng(1L))
  expect_length(all_of$samples, 5L)
  # an empty buffer has nothing to give
  expect_error(.dqnv_replay_buffer_sample(.dqnv_replay_buffer_new(4), 1,
                                          .ghc_rng(1L)), "buffer is empty")
})

# one training run, reused: each call costs real time, so the tests that
# need a converged learner share a single fit
trained <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      m <- mdp()
      cache <<- list(m = m, q = q_star(m),
                     fit = morie_dqnv(m$P, m$R, m$S, m$A, gamma = m$gamma,
                                      steps = 20000, alpha = 0.1, seed = 1))
    }
    cache
  }
})

test_that("the learner recovers the optimal action values and policy", {
  tr <- trained()
  Q <- tr$q
  # the optimal policy differs across states here, so a constant answer
  # cannot pass
  expect_identical(apply(Q, 1, which.max), c(1L, 1L, 2L, 2L))
  M <- if (is.matrix(tr$fit$Q)) tr$fit$Q else
    do.call(rbind, lapply(tr$fit$Q, as.numeric))
  expect_identical(dim(M), c(tr$m$S, tr$m$A))
  expect_lt(max(abs(M - Q)), 0.05)
  expect_identical(apply(M, 1, which.max), apply(Q, 1, which.max))
  # the reported greedy policy is 0-based, as action indices are here
  expect_identical(as.integer(unlist(tr$fit$greedy_policy)),
                   apply(Q, 1, which.max) - 1L)
  expect_true(tr$fit$used_replay)
  expect_true(tr$fit$used_target_network)
  expect_match(tr$fit$method, "Q|DQN|Mnih")
})

test_that("the residual falls over training", {
  tr <- trained()
  h <- as.numeric(unlist(tr$fit$residual_history))
  expect_gt(length(h), 1L)
  expect_lt(h[length(h)], h[1])
  expect_equal(h[length(h)], as.numeric(tr$fit$final_residual), tolerance = 1e-12)
  # and the learned Q really does nearly satisfy the Bellman equation
  M <- if (is.matrix(tr$fit$Q)) tr$fit$Q else
    do.call(rbind, lapply(tr$fit$Q, as.numeric))
  expect_lt(.dqnv_bellman_residual(as_list_Q(M), tr$m$P, tr$m$R, tr$m$gamma), 0.05)
})

test_that("replay and the target network can each be switched off", {
  m <- mdp()
  Q <- q_star(m)
  for (rep_ in c(TRUE, FALSE)) {
    for (tgt in c(TRUE, FALSE)) {
      fit <- morie_dqnv(m$P, m$R, m$S, m$A, gamma = m$gamma, steps = 4000,
                        alpha = 0.2, seed = 2, use_replay = rep_,
                        use_target = tgt)
      M <- if (is.matrix(fit$Q)) fit$Q else
        do.call(rbind, lapply(fit$Q, as.numeric))
      expect_true(all(is.finite(M)))
      # whichever devices are enabled, the policy comes out right
      expect_identical(apply(M, 1, which.max), apply(Q, 1, which.max))
      expect_identical(fit$used_replay, rep_)
      expect_identical(fit$used_target_network, tgt)
    }
  }
})

test_that("the state and action counts are validated", {
  m <- mdp()
  expect_error(morie_dqnv(m$P, m$R, 0, m$A), "at least one state and action")
  expect_error(morie_dqnv(m$P, m$R, m$S, 0), "at least one state and action")
})
