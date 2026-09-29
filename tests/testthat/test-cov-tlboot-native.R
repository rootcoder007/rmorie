# Bootstraps for TMLE (van der Laan & Rose 2018, Chap. 28): the naive
# row bootstrap and the exponential-weight multiplier bootstrap replayed
# on the shared stream, the targeted bootstrap, and the moment check.

tb_x <- c(1.2, 0.4, 2.5, -0.3, 1.1, 0.9, 1.8, 0.2)

test_that("the naive bootstrap resamples rows with floor(u n) indices", {
  r <- naive_bootstrap(as.list(tb_x), function(s) mean(unlist(s)), B = 6, seed = 3)
  e <- .ghc_rng(3)
  reps <- vapply(1:6, function(b) mean(tb_x[as.integer(.ghc_unif(e, 8L) * 8) %% 8 + 1]), 1)
  expect_equal(r$replicates, reps, tolerance = 1e-15)
  expect_equal(r$se, stats::sd(reps), tolerance = 1e-15)
  expect_error(morie_tlboot(list(1), mean), "at least 2")
  expect_error(morie_tlboot(as.list(tb_x), mean, B = 0), "positive integer")
})

test_that("the multiplier bootstrap reweights the IC with Exp(1) weights", {
  d <- tb_x - mean(tb_x)
  r <- multiplier_bootstrap(d, B = 5, seed = 4)
  e <- .ghc_rng(4)
  reps <- vapply(1:5, function(b) {
    w <- -log(pmax(.ghc_unif(e, 8L), 1e-12))
    sum(w * d) / sum(w)
  }, 1)
  expect_equal(r$replicates, reps, tolerance = 1e-15)
  expect_equal(r$influence_curve_se, stats::sd(d) / sqrt(8), tolerance = 1e-15)
  expect_equal(r$ratio, stats::sd(reps) / (stats::sd(d) / sqrt(8)), tolerance = 1e-14)
  big <- multiplier_bootstrap(stats::qnorm((1:200 - 0.5) / 200), B = 2000, seed = 1)
  # Bayesian-bootstrap weights reproduce sd/sqrt(n) up to Monte Carlo error
  expect_lt(abs(big$ratio - 1), 0.1)
  expect_error(morie_tlboot(NULL, NULL, method = "multiplier"), "requires an influence curve")
  expect_error(multiplier_bootstrap(1), "at least 2")
})

test_that("the targeted bootstrap draws datasets from the fitted law", {
  sampler <- function(e) .ghc_norm(e, 10L)
  r <- targeted_bootstrap(sampler, mean, B = 4, seed = 2)
  e <- .ghc_rng(2)
  reps <- vapply(1:4, function(b) mean(.ghc_norm(e, 10L)), 1)
  expect_equal(r$replicates, reps, tolerance = 1e-15)
  expect_equal(r$se, stats::sd(reps), tolerance = 1e-15)
  expect_equal(morie_tlboot(sampler, mean, B = 4, seed = 2, method = "targeted")$replicates, reps)
  expect_error(targeted_bootstrap(1, mean), "must be a function")
  expect_error(targeted_bootstrap(sampler, 1), "estimator must be")
  expect_error(targeted_bootstrap(sampler, mean, B = 1), "at least 2")
})

test_that("the moment check compares replicate mean and sd with targets", {
  v <- c(0.9, 1.1, 1.0, 1.3, 0.7)
  m <- moment_check(v, 1, 0.2)
  expect_equal(m$mean_error, abs(mean(v) - 1), tolerance = 1e-15)
  expect_equal(m$se_ratio, stats::sd(v) / 0.2, tolerance = 1e-15)
  expect_identical(m$first_two_moments_ok, abs(stats::sd(v) / 0.2 - 1) < 0.15)
  expect_error(moment_check(1, 0, 1), "at least 2")
})
