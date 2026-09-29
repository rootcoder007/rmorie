# Meta-RL evaluation loop (Wang et al. 2016): task families, history
# features, the tabular reference agent, and the episode loop replayed
# on the same random stream with a fixed-arm agent.

test_that("bandit task families draw arm probabilities from the stream", {
  t1 <- morie_mtdrl_bandit_tasks(3, 4, seed = 2)
  u <- .ghc_unif(.ghc_rng(2), 12L)
  expect_equal(unlist(t1), u, tolerance = 1e-15)
  tp <- morie_mtdrl_bandit_tasks(2, 3, seed = 5, structure = "paired")
  up <- .ghc_unif(.ghc_rng(5), 3L)
  expect_equal(t(sapply(tp, identity)), cbind(up, 1 - up), tolerance = 1e-15,
               ignore_attr = TRUE)
  set.seed(7)
  ref <- matrix(stats::runif(6), 2)
  t2 <- mtdrl_bandit_tasks(3, 2, seed = 7)
  expect_equal(unlist(t2), as.numeric(ref), tolerance = 1e-15)
  tq <- mtdrl_bandit_tasks(2, 2, seed = 7, structure = "paired")
  expect_equal(vapply(tq, sum, 1), c(1, 1))
  for (f in list(morie_mtdrl_bandit_tasks, mtdrl_bandit_tasks)) {
    expect_error(f(1), "at least 2 arms")
    expect_error(f(3, structure = "paired"), "2 arms")
    expect_error(f(2, structure = "chain"), "structure must be")
  }
})

test_that("history features are one-hot last action, last reward, step count", {
  h <- list(list(0L, 1), list(2L, 0.5))
  for (f in list(morie_mtdrl_history_features, mtdrl_history_features)) {
    expect_equal(f(h, 3), c(0, 0, 1, 0.5, 2))
    expect_equal(f(list(), 3), c(0, 0, 0, 0, 0))
  }
})

test_that("the tabular agent keeps running means and acts greedily", {
  ag <- morie_mtdrl_TabularHistoryAgent$new(n_arms = 3L, epsilon = 0, optimistic = 1)
  ag$observe(0, 1)
  ag$observe(0, 0)
  ag$observe(2, 0)
  expect_equal(ag$means, c(0.5, 1, 0))
  expect_identical(ag$counts, c(2L, 0L, 1L))
  expect_identical(ag$act(NULL, .ghc_rng(1)), 1L)
  ag$reset()
  expect_equal(ag$means, rep(1, 3))
  e <- mtdrl_TabularHistoryAgent(2, epsilon = 0, optimistic = 0.5)
  e$observe(1, 1)
  expect_equal(e$means, c(0.5, 1))
  expect_identical(e$act(NULL, function() 0.5), 1L)
  e$reset()
  expect_identical(e$counts, c(0L, 0L))
})

test_that("the evaluation loop replays on the shared stream", {
  tasks <- list(c(0.2, 0.7), c(0.9, 0.4), c(0.5, 0.5))
  fixed <- list(reset = function() NULL, act = function(f, e) 0L, observe = function(a, r) NULL)
  L <- 6
  r <- morie_mtdrl(tasks, fixed, episode_length = L, seed = 4)
  e <- .ghc_rng(4)
  rew <- matrix(0, 3, L)
  for (k in 1:3) for (t in 1:L) rew[k, t] <- as.numeric(.ghc_unif(e, 1L) < tasks[[k]][1])
  expect_equal(r$total_reward, sum(rew))
  expect_equal(r$episode_reward, rowSums(rew))
  expect_equal(r$reward_by_step, colMeans(rew), tolerance = 1e-15)
  expect_equal(r$regret, L * sum(vapply(tasks, function(p) max(p) - p[1], 1)), tolerance = 1e-12)
  # arm 0 is optimal in tasks 2 and 3 (a tie counts as optimal)
  expect_equal(r$optimal_action_rate, rep(2 / 3, L), tolerance = 1e-15)
  expect_equal(r$mean_reward, sum(rew) / (3 * L), tolerance = 1e-15)
  # the package's own agent is accepted
  ag <- morie_mtdrl_TabularHistoryAgent$new(n_arms = 2L)
  expect_identical(morie_mtdrl(tasks, ag, 5)$n_episodes, 3L)
  expect_error(morie_mtdrl(list(), fixed), "non-empty")
  expect_error(morie_mtdrl(list(1:2, 1:3), fixed), "every task")
  expect_error(morie_mtdrl(tasks, fixed, 0), "episode_length")
  expect_error(morie_mtdrl(tasks, list(act = identity)), "must provide reset")
  bad <- list(reset = function() NULL, act = function(f, e) 5L, observe = function(a, r) NULL)
  expect_error(morie_mtdrl(tasks, bad, 2), "outside 0..1")
})

test_that("the restored loop replays on R's seeded stream", {
  tasks <- list(c(0.3, 0.6), c(0.8, 0.1))
  fx <- new.env()
  fx$reset <- function() NULL
  fx$act <- function(f, rng) 1L
  fx$observe <- function(a, r) NULL
  r <- mtdrl_run(tasks, fx, episode_length = 4, seed = 9)
  set.seed(9)
  u <- stats::runif(8)
  rew <- as.numeric(u < rep(c(0.6, 0.1), each = 4))
  expect_equal(r$total_reward, sum(rew))
  expect_equal(r$regret, 4 * (0 + 0.7), tolerance = 1e-12)
  expect_equal(r$optimal_action_rate, rep(0.5, 4))
  ag <- mtdrl_TabularHistoryAgent(2)
  expect_identical(mtdrl_run(tasks, ag, 3)$n_episodes, 2L)
  expect_error(mtdrl_run(tasks, new.env(), 3), "must provide reset")
  expect_match(mtdrl_cheatsheet(), "RESET", fixed = TRUE)
})
