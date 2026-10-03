# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P13 (continued): the HKSJ interval must agree with research/lean/P13HKSJ.lean.

test_that("hksj_wider_iff: HKSJ variance >= Wald variance exactly when q >= 1; q and Q from first principles", {
  set.seed(13)
  seen <- c(FALSE, FALSE)
  for (rep in 1:60) {
    k <- sample(2:7, 1)
    v <- runif(k, 0.005, 0.05)
    est <- rnorm(k, -0.2, sqrt(v) * sample(c(0.3, 1, 3), 1))
    for (method in c("DL", "REML")) {
      h <- morie_meta_hksj(est, v, tau2 = method)
      w <- 1 / (v + h$tau2)
      mu <- sum(w * est) / sum(w)
      Q <- sum(w * (est - mu)^2)
      expect_equal(h$estimate, mu, tolerance = 1e-12)
      expect_equal(h$Q, Q, tolerance = 1e-12)
      expect_equal(h$q, Q / (k - 1), tolerance = 1e-12)
      expect_equal(h$hksj$variance, h$q / sum(w), tolerance = 1e-12)
      expect_equal(h$wald$variance, 1 / sum(w), tolerance = 1e-12)
      expect_equal(h$wider_than_wald, h$q >= 1)
      expect_equal(h$hksj$variance >= h$wald$variance, h$q >= 1)      # hksj_wider_iff
      expect_equal(h$hksj$df, k - 1)
      tq <- qt(0.975, k - 1)
      expect_equal(unname(h$hksj$ci), c(mu - tq * h$hksj$se, mu + tq * h$hksj$se), tolerance = 1e-12)
      expect_gte(h$tau2, 0)
      seen[1 + (h$q >= 1)] <- TRUE
    }
  }
  expect_true(all(seen))
})

test_that("Q_eq_zero_iff: degenerate exactly when every site equals the pooled value", {
  h <- morie_meta_hksj(c(0.2, 0.2, 0.2, 0.2), c(0.01, 0.02, 0.03, 0.04))
  expect_true(h$degenerate)
  expect_equal(h$hksj$se, 0)
  expect_equal(unname(h$hksj$ci), c(0.2, 0.2))
  h2 <- morie_meta_hksj(c(0.2, 0.2, 0.2, 0.21), c(0.01, 0.02, 0.03, 0.04))
  expect_false(h2$degenerate)
  expect_gt(h2$Q, 0)
})

test_that("hksj_equal_weights: equal variances give the one-sample t variance s^2/k, for DL and REML", {
  set.seed(5)
  for (rep in 1:20) {
    k <- sample(2:8, 1)
    est <- rnorm(k)
    v <- rep(runif(1, 0.01, 1), k)
    for (method in c("DL", "REML")) {
      h <- morie_meta_hksj(est, v, tau2 = method)
      expect_true(h$equal_weights)
      expect_equal(h$hksj$variance, var(est) / k, tolerance = 1e-12)
      expect_equal(h$t_variance, var(est) / k, tolerance = 1e-12)
      # the HKSJ interval is then the t interval of the plain mean
      tt <- t.test(est)
      expect_equal(unname(h$hksj$ci), unname(as.numeric(tt$conf.int)), tolerance = 1e-10)
      expect_equal(h$estimate, mean(est), tolerance = 1e-12)
    }
  }
  expect_false(morie_meta_hksj(c(1, 2, 3), c(1, 2, 3))$equal_weights)
})

test_that("REML tau2 maximises the restricted log-likelihood on a grid and is zero when the profile peaks at zero", {
  ll <- function(t, y, v) {
    w <- 1 / (v + t); mu <- sum(w * y) / sum(w)
    -0.5 * sum(log(v + t)) - 0.5 * log(sum(w)) - 0.5 * sum(w * (y - mu)^2)
  }
  est <- c(-0.25, -0.10, -0.40, 0.05, -0.30); v <- c(0.010, 0.020, 0.015, 0.030, 0.012)
  r <- morie_meta_hksj(est, v, tau2 = "REML")
  expect_equal(r$tau2_method, "REML")
  grid <- seq(0, 0.2, by = 1e-4)
  expect_gte(ll(r$tau2, est, v), max(vapply(grid, ll, numeric(1), y = est, v = v)) - 1e-9)
  expect_equal(r$tau2, 0.00290803591431826, tolerance = 1e-6)  # a line search on a flat profile: platforms differ at 1e-7
  hom <- morie_meta_hksj(c(0.10, 0.11, 0.09, 0.10), c(0.05, 0.05, 0.05, 0.05), tau2 = "REML")
  expect_equal(hom$tau2, 0)
})

test_that("input checks", {
  expect_error(morie_meta_hksj(1, 1), "at least two")
  expect_error(morie_meta_hksj(c(1, 2), c(1, 2, 3)), "equal length")
  expect_error(morie_meta_hksj(c(1, 2), c(1, -1)), "positive")
  expect_error(morie_meta_hksj(c(1, 2), c(1, 1), level = 1), "level")
  expect_error(morie_meta_hksj(c(1, 2), c(1, 1), tau2 = "ML"), "arg")
})
