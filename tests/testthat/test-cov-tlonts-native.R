# Online TMLE for a single time series (van der Laan & Rose 2018, Chap.
# 19): lagged summaries, stochastic interventions on a node subset,
# martingale variance and check, and the running estimator.

test_that("lag summaries hold the last L values before t, zero-padded", {
  v <- c(5, 6, 7, 8, 9)
  expect_equal(lag_summary(v, 3, 2), c(6, 7))
  expect_equal(lag_summary(v, 1, 3), c(0, 0, 5))
  expect_equal(lag_summary(v, 0, 2), c(0, 0))
  expect_error(lag_summary(v, 2, -1), "non-negative")
})

test_that("stochastic interventions shift or randomise only the chosen nodes", {
  a <- c(0, 1, 0, 1, 1)
  s <- stochastic_intervention(a, c(3, 1), shift = 0.5)
  expect_equal(s$intervened, c(0, 1.5, 0, 1.5, 1))
  expect_identical(s$nodes, c(1L, 3L))
  p <- stochastic_intervention(a, 4, prob = 0.3)
  expect_equal(p$intervened, c(0, 1, 0, 1, 0.3))
  expect_identical(p$kind, "bernoulli")
  expect_error(stochastic_intervention(a, 5, shift = 1), "outside the series")
  expect_error(stochastic_intervention(a, 1), "exactly one")
  expect_error(stochastic_intervention(a, 1, shift = 1, prob = 0.2), "exactly one")
})

test_that("martingale variance is mean(D^2) and the check correlates D with the past", {
  D <- c(0.2, -0.5, 0.1, 0.4, -0.3)
  mv <- martingale_variance(D)
  expect_equal(mv$variance, mean(D^2), tolerance = 1e-15)
  expect_equal(mv$se, sqrt(mean(D^2) / 5), tolerance = 1e-15)
  expect_error(martingale_variance(1), "at least 2")
  past <- c(1, 0, 2, 3, 1)
  mc <- martingale_check(D, past)
  expect_equal(mc$correlation, stats::cor(D, past), tolerance = 1e-14)
  expect_identical(mc$is_martingale, abs(stats::cor(D, past)) < 0.2)
  expect_error(martingale_check(D, past[-1]), "5 influence terms but 4")
})

test_that("the online estimator averages psi_t and scores H (Y - Q) + psi_t - psi", {
  set.seed(29)
  T <- 60
  Z <- matrix(stats::rnorm(T * 2), T)
  a <- stats::rbinom(T, 1, 0.4)
  y <- 0.5 * a + Z[, 1] + stats::rnorm(T, sd = 0.3)
  Q <- function(av, z) 0.4 * av + z[1]
  g <- function(z) stats::plogis(0.2 * z[2])
  r <- online_tmle_series(y, a, Z, Q, g, target_prob = 0.7, burn_in = 10)
  idx <- 11:T
  gv <- stats::plogis(0.2 * Z[idx, 2])
  psi_t <- 0.7 * (0.4 + Z[idx, 1]) + 0.3 * Z[idx, 1]
  run <- cumsum(psi_t) / seq_along(psi_t)
  H <- ifelse(a[idx] == 1, 0.7 / gv, 0.3 / (1 - gv))
  D <- H * (y[idx] - (0.4 * a[idx] + Z[idx, 1])) + psi_t - run
  expect_equal(r$path, run, tolerance = 1e-14)
  expect_equal(r$psi, run[length(run)], tolerance = 1e-15)
  expect_equal(r$se, sqrt(mean(D^2) / length(D)), tolerance = 1e-14)
  expect_identical(r$T_scored, 50L)
  expect_equal(morie_tlonts(y, a, Z, Q, g, 0.7, 10)$psi, r$psi)
  expect_error(online_tmle_series(y, a[-1], Z, Q, g, 0.5), "differ in length")
  expect_error(online_tmle_series(y, a, Z, Q, g, 0.5, burn_in = T), "burn_in must lie")
  expect_error(online_tmle_series(y, a, Z, Q, function(z) 1, 0.5), "left \\(0,1\\)")
})
