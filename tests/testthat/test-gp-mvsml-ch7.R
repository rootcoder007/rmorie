# MVSML chapter 7: ordinal models.

test_that("eq (7.1) probabilities match the book formula", {
  p <- morie_ordinal_probs(0, c(-1, 0.5))
  expect_equal(sum(p), 1, tolerance = 1e-12)
  expect_equal(p[1], pnorm(-1), tolerance = 1e-12)
  expect_equal(p[2], pnorm(0.5) - pnorm(-1), tolerance = 1e-12)
  expect_equal(p[3], 1 - pnorm(0.5), tolerance = 1e-12)
  # latent is L = -x'beta + eps, so a larger eta moves mass down
  hi <- morie_ordinal_probs(2, c(-1, 0.5))
  expect_gt(hi[1], p[1])
  expect_lt(hi[3], p[3])
})

test_that("the logistic link matches the p.210 formula", {
  p <- morie_ordinal_probs(0.3, c(-0.5, 1), link = "logistic")
  expect_equal(p[1], plogis(-0.5 + 0.3), tolerance = 1e-12)
  expect_equal(p[2], plogis(1 + 0.3) - plogis(-0.5 + 0.3),
               tolerance = 1e-12)
  expect_equal(sum(p), 1, tolerance = 1e-12)
})

test_that("the binary case reduces to probit regression (p.210)", {
  p <- morie_ordinal_probs(0.4, 0)
  expect_equal(length(p), 2L)
  expect_equal(p[1], pnorm(0.4), tolerance = 1e-12)
})

test_that("truncated normal draws stay inside the interval", {
  set.seed(4)
  d <- replicate(500, morie_rtruncnorm(0, 1, 0.5, 1.5))
  expect_true(all(d >= 0.5 & d <= 1.5))
  expect_equal(mean(d), 0.90, tolerance = 0.06)
})

test_that("the ordinal probit Gibbs sampler recovers a signal", {
  set.seed(11)
  n <- 120
  X <- matrix(rnorm(n), ncol = 1)
  lat <- -1.5 * X[, 1] + rnorm(n)
  y <- ifelse(lat < -0.5, 1L, ifelse(lat < 0.7, 2L, 3L))
  r <- morie_ordinal_probit_gibbs(y, X, n_iter = 900L,
                                        burn_in = 300L)
  expect_equal(r$n_categories, 3L)
  expect_lt(r$gamma[1], r$gamma[2])
  expect_gt(r$beta[1], 0.5)
})
