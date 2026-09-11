# Anchors for REINFORCE (Williams 1992, Machine Learning 8, 229-256).
#
# The module's central claim is Theorem 1: the expected update has a
# non-negative inner product with the gradient of expected reward, for
# any baseline. For a Bernoulli-logistic unit with two outcomes that is a
# closed form, so it is asserted exactly rather than sampled:
#
#   E{dw} = alpha * p * (1 - p) * (r1 - r0)  =  alpha * dE{r}/dw
#
# and it does not depend on the baseline b at all, which is the point of
# equation 10. reward_fn is called as reward_fn(y, x).

test_that("the expected update is the closed form and ignores the baseline", {
  for (p in c(0.05, 0.1, 0.5, 0.9, 0.95)) {
    for (b in c(0, -5, 10, 1e3)) {
      for (rr in list(c(0, 1), c(1, 0), c(-2, 3), c(4, 4))) {
        got <- morie_reinfc_expected_update(p, r0 = rr[1], r1 = rr[2],
                                            alpha = 2, b = b)
        want <- 2 * p * (1 - p) * (rr[2] - rr[1])
        expect_equal(got[[1]], want, tolerance = 1e-12)
        # REINFORCE is unbiased: the expected update IS alpha times the
        # gradient of expected reward
        expect_equal(got[[1]], got[[2]], tolerance = 1e-12)
      }
    }
  }
})

test_that("Theorem 1 holds: the update never opposes the gradient", {
  for (p in c(0.1, 0.4, 0.6, 0.9)) {
    for (b in c(-3, 0, 7)) {
      for (rr in list(c(0, 1), c(1, 0), c(-5, -1), c(2, -2))) {
        got <- morie_reinfc_expected_update(p, rr[1], rr[2], alpha = 1, b = b)
        # dE{r}/dw = p(1-p)(r1 - r0)
        grad <- p * (1 - p) * (rr[2] - rr[1])
        expect_gte(got[[1]] * grad, 0)
        # and the update's sign is the gradient's sign
        if (rr[2] != rr[1]) expect_identical(sign(got[[1]]), sign(grad))
      }
    }
  }
})

test_that("the expected update rejects a degenerate probability", {
  expect_error(morie_reinfc_expected_update(0, 0, 1), "strictly in")
  expect_error(morie_reinfc_expected_update(1, 0, 1), "strictly in")
  expect_error(morie_reinfc_expected_update(-0.1, 0, 1), "strictly in")
  expect_error(morie_reinfc_expected_update(1.1, 0, 1), "strictly in")
})

test_that("the logistic is the closed form and does not overflow", {
  expect_equal(.reinfc_logistic(0), 0.5)
  expect_equal(.reinfc_logistic(2), 1 / (1 + exp(-2)), tolerance = 1e-15)
  expect_equal(.reinfc_logistic(-2), 1 - .reinfc_logistic(2), tolerance = 1e-14)
  # the two branches exist so that neither tail overflows
  expect_equal(.reinfc_logistic(800), 1)
  expect_equal(.reinfc_logistic(-800), 0)
  expect_false(is.nan(.reinfc_logistic(-800)))
  expect_true(all(vapply(seq(-50, 50, by = 5),
                         function(s) .reinfc_logistic(s) >= 0 &&
                                     .reinfc_logistic(s) <= 1, logical(1))))
})

# reward 1 for action 1, 0 otherwise: the optimal policy is p = 1
reward_one <- function(y, x) if (as.numeric(y)[1] == 1) 1 else 0
reward_zero <- function(y, x) if (as.numeric(y)[1] == 0) 1 else 0

test_that("a Bernoulli unit climbs toward the rewarded action", {
  up <- morie_reinfc(reward_one, p = 0.5, unit = "bernoulli",
                     trials = 300L, seed = 1L)
  expect_gt(unlist(up$estimate)[1], 0.9)
  expect_gt(up$mean_reward_last, up$mean_reward_first)
  expect_identical(up$n_trials, 300L)
  expect_length(up$rewards, 300L)
  expect_length(up$trajectory, 300L)
  expect_match(up$method, "Williams")

  # and away from it when the reward is reversed
  dn <- morie_reinfc(reward_zero, p = 0.5, unit = "bernoulli",
                     trials = 300L, seed = 1L)
  expect_lt(unlist(dn$estimate)[1], 0.1)
  expect_gt(dn$mean_reward_last, dn$mean_reward_first)
})

test_that("a Bernoulli unit already at the optimum stays there", {
  r <- morie_reinfc(reward_one, p = 0.98, unit = "bernoulli",
                    trials = 200L, seed = 2L)
  expect_gt(unlist(r$estimate)[1], 0.9)
  expect_gt(r$mean_reward_last, 0.9)
})

test_that("a Gaussian unit moves its mean toward the reward's optimum", {
  # The reward has to be bounded: Williams' convergence argument assumes
  # it, and with -(y - c)^2 the mean update is quadratic in mu and runs
  # away. A Gaussian bump is bounded and peaks where we want it.
  peak_at <- function(c) function(y, x) exp(-0.5 * (as.numeric(y)[1] - c)^2)
  r <- morie_reinfc(peak_at(2), mu = 0, sigma = 1, unit = "gaussian",
                    trials = 600L, alpha = 0.2, seed = 1L)
  est <- unlist(r$estimate)
  expect_gt(est[1], 0.3)                 # started at 0, peak is at 2
  expect_gt(r$mean_reward_last, r$mean_reward_first)
  expect_true(est[2] > 0)                # sigma stays positive
  expect_equal(r$mu, est[1], tolerance = 1e-12)
  expect_equal(r$sigma, est[2], tolerance = 1e-12)

  # a peak below the start pulls the mean the other way
  d <- morie_reinfc(peak_at(-3), mu = 0, sigma = 1, unit = "gaussian",
                    trials = 600L, alpha = 0.2, seed = 1L)
  expect_lt(unlist(d$estimate)[1], est[1])
})

test_that("an unbounded reward is reported as divergence, not returned as NaN", {
  runaway <- function(y, x) -(as.numeric(y)[1] + 3)^2
  expect_error(
    morie_reinfc(runaway, mu = 0, sigma = 1, unit = "gaussian",
                 trials = 400L, alpha = 0.05, seed = 1L),
    "diverged at trial"
  )
})

test_that("a logistic unit fits weights over its inputs", {
  set.seed(3)
  X <- matrix(rnorm(40), 20, 2)
  # reward the action only when the first feature is positive
  rf <- function(y, x) {
    good <- as.numeric(x)[1] > 0
    if (identical(as.numeric(y)[1] == 1, good)) 1 else 0
  }
  r <- morie_reinfc(rf, x = X, w = c(0, 0), unit = "bernoulli-logistic",
                    trials = 400L, alpha = 0.2, seed = 1L)
  expect_false(is.null(r$weights))
  expect_length(unlist(r$weights), 2L)
  expect_gt(r$mean_reward_last, r$mean_reward_first)
  # the weight on the informative feature should have grown
  expect_gt(unlist(r$weights)[1], 0)
})

test_that("every baseline and mode runs and learns", {
  for (b in c("none", "comparison", "mean")) {
    for (m in c("immediate", "episodic")) {
      r <- morie_reinfc(reward_one, p = 0.5, unit = "bernoulli", baseline = b,
                        mode = m, trials = 200L, episode_length = 5L, seed = 1L)
      expect_identical(r$n_trials, 200L)
      expect_true(all(is.finite(unlist(r$rewards))))
      # the baseline series is reported and finite
      expect_true(all(is.finite(unlist(r$baseline))))
      expect_gt(unlist(r$estimate)[1], 0.5)
    }
  }
})

test_that("a run is reproducible from its seed", {
  a <- morie_reinfc(reward_one, p = 0.5, unit = "bernoulli", trials = 100L, seed = 5L)
  b <- morie_reinfc(reward_one, p = 0.5, unit = "bernoulli", trials = 100L, seed = 5L)
  d <- morie_reinfc(reward_one, p = 0.5, unit = "bernoulli", trials = 100L, seed = 6L)
  expect_equal(unlist(a$trajectory), unlist(b$trajectory), tolerance = 1e-15)
  expect_equal(unlist(a$rewards), unlist(b$rewards), tolerance = 1e-15)
  expect_false(isTRUE(all.equal(unlist(a$rewards), unlist(d$rewards))))
})

test_that("the baseline series follows its rule", {
  rewards <- c(1, 0, 1, 1, 0)
  # no baseline is a zero series
  expect_equal(.reinfc_baseline_series(rewards, "none", 0.5), rep(0, 5))
  # the mean baseline is the running mean of what came before
  mb <- .reinfc_baseline_series(rewards, "mean", 0.5)
  expect_length(mb, 5L)
  expect_true(all(is.finite(mb)))
  expect_equal(mb[1], 0)
  # a reinforcement-comparison baseline is an exponential trace
  cb <- .reinfc_baseline_series(rewards, "comparison", 0.5)
  expect_length(cb, 5L)
  expect_true(all(is.finite(cb)))
})

test_that("matrix coercion accepts the documented shapes", {
  m <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_equal(.reinfc_as_matrix(m, "x"), m)
  expect_equal(.reinfc_as_matrix(as.data.frame(m), "x"), m, ignore_attr = TRUE)
  expect_equal(.reinfc_as_matrix(c(1, 2, 3), "x"), matrix(c(1, 2, 3), nrow = 1))
})
