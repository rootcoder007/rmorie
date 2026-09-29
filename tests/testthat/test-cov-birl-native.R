# Coverage tests for R/birl_native.R (Ramachandran and Amir 2007): policy
# evaluation, Q values, policy iteration, the Boltzmann likelihood, the
# four priors, one PolicyWalk step replayed on the LCG stream, and birl.

bi_T <- list(
  list(c(0.8, 0.2, 0), c(0.1, 0.1, 0.8)),
  list(c(0.5, 0.5, 0), c(0, 0.3, 0.7)),
  list(c(0.2, 0, 0.8), c(0.9, 0, 0.1))
)
bi_P <- function(pol) t(vapply(1:3, function(s) bi_T[[s]][[pol[s]]], numeric(3)))

test_that("policy evaluation, Q values and policy iteration", {
  R <- c(0.5, -1, 1)
  V <- policy_values(bi_T, R, 0.9, c(1, 2, 1))
  expect_equal(V, as.numeric(solve(diag(3) - 0.9 * bi_P(c(1, 2, 1)), R)), tolerance = 1e-12)
  Q <- q_values(bi_T, R, 0.9, V)
  expect_equal(Q[2, 1], R[2] + 0.9 * sum(bi_T[[2]][[1]] * V), tolerance = 1e-12)
  pi <- policy_iteration(bi_T, R, 0.9)
  pols <- as.matrix(expand.grid(1:2, 1:2, 1:2))
  vals <- apply(pols, 1, function(p) sum(policy_values(bi_T, R, 0.9, p)))
  expect_equal(pi$policy, as.integer(pols[which.max(vals), ]))
  expect_equal(pi$V, policy_values(bi_T, R, 0.9, pi$policy), tolerance = 1e-12)
  expect_true(all(apply(pi$Q, 1, which.max) == pi$policy))
  expect_error(policy_iteration(bi_T, R[-1], 0.9), "one reward per state")
  expect_error(policy_iteration(bi_T, R, 0.9, policy = 1), "wrong length")
  expect_error(policy_values(bi_T, R, 0.9, 1:2), "one entry per state")
  expect_error(.mdp(list(), 0.9), "empty")
  expect_error(.mdp(list(list()), 0.9), "no actions")
  expect_error(.mdp(list(list(c(0.5, 0.5)), list(c(1, 0), c(0, 1))), 0.9), "same actions")
  expect_error(.mdp(list(list(c(1, 0))), 0.9), "wrong length")
  expect_error(.mdp(list(list(0.5)), 0.9), "probability distributions")
  expect_error(.mdp(bi_T, 1), "gamma must be in")
  expect_error(.solve(matrix(0, 2, 2), 1:2), "singular")
})

test_that("Boltzmann likelihood and the four reward priors", {
  Q <- rbind(c(1, 2), c(0.5, -0.5), c(0, 0))
  obs <- list(c(1, 2), c(2, 1), c(3, 2))
  ref <- sum(vapply(obs, function(o) 2 * Q[o[1], o[2]] - log(sum(exp(2 * Q[o[1], ]))), 0))
  expect_equal(.birl_log_likelihood(Q, obs, alpha = 2), ref, tolerance = 1e-12)
  expect_error(.birl_log_likelihood(Q, obs, 0), "alpha must be positive")
  expect_error(.birl_log_likelihood(Q, list()), "no observations")
  expect_error(.birl_log_likelihood(Q, list(c(4, 1))), "out of range")
  R <- c(0.5, -1, 0.25)
  expect_equal(log_prior(R), 0)
  expect_equal(log_prior(R, r_max = 0.75), -Inf)
  expect_equal(log_prior(R, "gaussian", 2), -sum(R^2) / 8)
  expect_equal(log_prior(R, "laplacian", 0.5), -sum(abs(R)) / 0.5)
  expect_equal(log_prior(R, "ising", J = 0.3, H = 0.1), -(0.3 * (R[1] * R[2] + R[2] * R[3]) + 0.1 * sum(R)))
  expect_equal(log_prior(R, "ising", neighbours = rbind(c(1, 3))), -0.1 * R[1] * R[3])
  expect_error(log_prior(R, "beta"), "prior must be one of")
  expect_error(log_prior(R, "gaussian", 0), "scale must be positive")
})

test_that("one PolicyWalk step replays on the LCG stream", {
  obs <- list(c(1, 2), c(2, 2), c(3, 1))
  R0 <- c(0.5, -0.25, 0.75)
  w <- policy_walk(bi_T, obs, 0.9, n_iter = 1, burn = 0, seed = 5, R0 = R0)
  rnd <- .rng(8L)
  s <- as.integer(rnd() * 3) + 1L
  step <- if (rnd() < 0.5) 0.25 else -0.25
  cand <- R0
  cand[s] <- round((cand[s] + step) / 0.25) * 0.25
  sc <- function(r) .birl_log_likelihood(policy_iteration(bi_T, r, 0.9)$Q, obs) + log_prior(r, r_max = 1)
  if (abs(cand[s]) > 1) {
    expect_equal(w$samples[1, ], R0)
  } else {
    acc <- sc(cand) > sc(R0) || log(rnd()) < sc(cand) - sc(R0)
    expect_equal(w$samples[1, ], if (acc) cand else R0)
    expect_equal(w$acceptance, as.numeric(acc))
  }
  long <- policy_walk(bi_T, obs, 0.9, n_iter = 60, seed = 2)
  expect_equal(dim(long$samples), c(30L, 3L))
  expect_true(all(abs(long$samples / 0.25 - round(long$samples / 0.25)) < 1e-9))
  expect_true(all(abs(long$samples) <= 1 + 1e-12))
  expect_equal(policy_walk(bi_T, obs, 0.9, n_iter = 60, seed = 2), long)
  expect_error(policy_walk(bi_T, obs, 0.9, delta = 0), "delta must be positive")
  expect_error(policy_walk(bi_T, obs, 0.9, n_iter = 0), "n_iter must be positive")
  expect_error(policy_walk(bi_T, obs, 0.9, n_iter = 5, burn = 5), "burn must be less")
})

test_that("birl reports the optimal policy of the posterior mean reward", {
  obs <- list(c(1, 2), c(2, 2), c(3, 1), c(1, 2))
  b <- birl(bi_T, obs, n_iter = 80, seed = 3, prior = "gaussian")
  expect_equal(b$reward_mean, colMeans(b$samples))
  expect_equal(b$reward_sd, apply(b$samples, 2, sd), tolerance = 1e-12)
  expect_equal(b$policy, policy_iteration(bi_T, b$reward_mean, 0.9)$policy)
  expect_equal(b$n_samples, 40L)
  expect_identical(bayesian_irl, birl)
  expect_identical(bayesianirl, birl)
  expect_identical(morie_birl, birl)
})
