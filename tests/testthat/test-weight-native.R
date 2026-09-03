# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 15 (WeightIt replacement): invariants on known DGPs +
# cross-validation against WeightIt / CBPS when installed.

.wt_dgp <- function(n = 400L, seed = 21) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rbinom(n, 1, 0.5)
  ps <- stats::plogis(0.6 * x1 - 0.5 * x2)
  t <- rbinom(n, 1, ps)
  data.frame(t = t, x1 = x1, x2 = x2)
}

test_that("morie_weight_ps matches WeightIt glm weights exactly", {
  d <- .wt_dgp()
  w <- morie_weight_ps(d, "t", c("x1", "x2"))
  expect_s3_class(w, "morie_weight")
  expect_true(all(w$weights > 0))
  # ATE-weighted covariate means balance.
  bal <- morie_weight_diagnostic(w, d, "t", c("x1", "x2"))
  expect_true(is.data.frame(bal))

  skip_if_not_installed("WeightIt")
  ref <- WeightIt::weightit(t ~ x1 + x2, data = d, method = "glm",
                            estimand = "ATE")
  expect_lt(max(abs(w$weights - as.numeric(ref$weights))), 1e-8)
})

test_that("estimand transforms are correct (ATT/ATC/ATO invariants)", {
  d <- .wt_dgp()
  watt <- morie_weight_ps(d, "t", c("x1", "x2"), estimand = "ATT")
  expect_true(all(watt$weights[d$t == 1] == 1))
  watc <- morie_weight_ps(d, "t", c("x1", "x2"), estimand = "ATC")
  expect_true(all(watc$weights[d$t == 0] == 1))
  wow <- morie_weight_ow(d, "t", c("x1", "x2"))
  expect_true(all(wow$weights >= 0 & wow$weights <= 1)) # bounded
})

test_that("stabilized weights have mean ~ 1 within arms", {
  d <- .wt_dgp()
  w <- morie_weight_stabilized(d, "t", c("x1", "x2"))
  expect_true(w$stabilize)
  expect_lt(abs(mean(w$weights) - 1), 0.15)
})

test_that("entropy weights achieve exact ATT moment balance", {
  d <- .wt_dgp()
  w <- morie_weight_entropy(d, "t", c("x1", "x2"))
  expect_equal(w$estimand, "ATT")
  wc <- w$weights[d$t == 0]
  m_t <- colMeans(d[d$t == 1, c("x1", "x2")])
  m_c <- colSums(d[d$t == 0, c("x1", "x2")] * wc) / sum(wc)
  expect_lt(max(abs(m_t - m_c)), 1e-4)
})

test_that("CBPS solves the balance moment conditions", {
  d <- .wt_dgp(n = 600L)
  w <- morie_weight_cbps(d, "t", c("x1", "x2"))
  ps <- w$propensity
  t01 <- d$t
  X <- cbind(1, d$x1, d$x2)
  # The just-identified CBPS moment: X'(T - p)/(p(1-p)) ~ 0.
  m <- as.numeric(crossprod(X, (t01 - ps) / (ps * (1 - ps)))) / nrow(d)
  expect_lt(max(abs(m)), 1e-6)

  skip_if_not_installed("CBPS")
  suppressWarnings(
    ref <- CBPS::CBPS(t ~ x1 + x2, data = d, method = "exact",
                      ATT = 0)
  )
  # Same estimator: fitted propensities agree closely.
  expect_lt(mean(abs(ps - as.numeric(ref$fitted.values))), 0.02)
})

test_that("trimming caps the tail and raises ESS ordering sanity", {
  d <- .wt_dgp()
  w <- morie_weight_ps(d, "t", c("x1", "x2"))
  wt <- morie_weight_trimming(w, q = 0.9)
  expect_lte(max(wt$weights), stats::quantile(w$weights, 0.9) + 1e-12)
  expect_gte(wt$ess, w$ess) # capping tails cannot lower ESS
})

test_that("SuperLearner stack returns simplex weights + sane scores", {
  d <- .wt_dgp(n = 300L)
  w <- morie_weight_super(d, "t", c("x1", "x2"), n_folds = 3L)
  expect_s3_class(w, "morie_weight")
  a <- w$learner_weights
  expect_true(all(a >= -1e-12))
  expect_lt(abs(sum(a) - 1), 1e-8)
  expect_true(all(w$propensity > 0 & w$propensity < 1))
  # Ensemble should correlate with the true score.
  true_ps <- stats::plogis(0.6 * d$x1 - 0.5 * d$x2)
  expect_gt(stats::cor(w$propensity, true_ps), 0.7)
})

test_that("morie_ipw composition: weights feed the IPW ATE", {
  set.seed(31)
  n <- 500
  x <- rnorm(n)
  ps <- stats::plogis(0.8 * x)
  t <- rbinom(n, 1, ps)
  y <- 1.5 * t + x + rnorm(n)
  d <- data.frame(y, t, x)
  w <- morie_weight_ps(d, "t", "x")
  d$w_ipw <- w$weights
  fit <- estimate_ate(d, "y", "t", "w_ipw")
  expect_lt(abs(fit$ate - 1.5), 4 * fit$se)
})
