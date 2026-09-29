# Coverage tests for R/bnsadt_native.R (Andrews and Kasy 2019): the
# step publication probability, the selection-corrected likelihood, the
# median-unbiased estimator and the adversarial bound.

bn_x <- c(0.8, 2.5, 0.3, 1.9, 3.1, -0.4, 2.2, 0.9)
bn_s <- c(0.4, 0.5, 0.3, 0.6, 0.7, 0.4, 0.5, 0.45)

test_that("publication probability steps and group counts", {
  expect_equal(morie_bnsadt_p(2.5, params = 0.3), 1)
  expect_equal(morie_bnsadt_p(1, params = 0.3), 0.3)
  expect_equal(morie_bnsadt_p(-2.5, params = 0.3), 1)
  expect_equal(morie_bnsadt_p(1.8, "symmetric_step2", c(0.2, 0.6)), 0.6)
  expect_equal(morie_bnsadt_p(-0.5, "signed_step", c(0.1, 0.2, 0.3)), 0.2)
  z <- bn_x / bn_s
  expect_equal(morie_bnsadt_group_counts(bn_x, bn_s, "symmetric_step"), sum(abs(z) < 1.96))
  expect_equal(morie_bnsadt_group_counts(bn_x, bn_s, "none"), integer(0))
})

test_that("selection-corrected log-likelihood", {
  mu <- 0.9
  tau <- 0.5
  b <- 0.4
  ll <- 0
  for (i in seq_along(bn_x)) {
    s <- sqrt(tau^2 + bn_s[i]^2)
    p <- if (abs(bn_x[i] / bn_s[i]) >= 1.96) 1 else b
    ep <- 1 - (1 - b) * (pnorm((1.96 * bn_s[i] - mu) / s) - pnorm((-1.96 * bn_s[i] - mu) / s))
    ll <- ll + log(p) + dnorm(bn_x[i], mu, s, log = TRUE) - log(ep)
  }
  expect_equal(morie_bnsadt_loglik(bn_x, bn_s, mu, tau, params = b), ll, tolerance = 1e-12)
  expect_equal(morie_bnsadt_loglik(bn_x, bn_s, mu, tau, "none"), sum(dnorm(bn_x, mu, sqrt(tau^2 + bn_s^2), log = TRUE)), tolerance = 1e-12)
  expect_equal(morie_bnsadt_loglik(bn_x, bn_s, mu, tau, params = 0), -Inf)
})

test_that("median-unbiased estimator solves the truncated-normal median equation", {
  expect_equal(morie_bnsadt_median_unbiased(1.3, 0.5, "none"), 1.3, tolerance = 1e-8)
  th <- morie_bnsadt_median_unbiased(2.2, 1, params = 0.2)
  cdf <- function(x, th, b) {
    f <- function(a, c) pnorm((c - th)) - pnorm((a - th))
    num <- if (x <= -1.96) f(-Inf, x) else if (x <= 1.96) f(-Inf, -1.96) + b * f(-1.96, x) else f(-Inf, -1.96) + b * f(-1.96, 1.96) + f(1.96, x)
    den <- f(-Inf, -1.96) + b * f(-1.96, 1.96) + f(1.96, Inf)
    num / den
  }
  expect_equal(cdf(2.2, th, 0.2), 0.5, tolerance = 1e-8)
  # selection toward significance shrinks the corrected estimate
  expect_lt(th, 2.2)
})

test_that("fit and the adversarial bound", {
  f <- morie_bnsadt_fit(bn_x, bn_s, "symmetric_step")
  expect_equal(f$loglik, morie_bnsadt_loglik(bn_x, bn_s, f$mu, f$tau, "symmetric_step", f$betas), tolerance = 1e-9)
  expect_gte(f$loglik, morie_bnsadt_loglik(bn_x, bn_s, mean(bn_x), 0.5, "symmetric_step", 0.5))
  expect_error(morie_bnsadt_fit(bn_x, bn_s, "cubic"), "family must be one of")
  r <- morie_bnsadt(bn_x, bn_s, grid = c(1, 0.5, 0.1))
  z <- bn_x / bn_s
  expect_equal(r$target, bn_x[which.max(abs(z))])
  expect_equal(r$estimate, morie_bnsadt_median_unbiased(r$target, r$target_se, "symmetric_step", r$betas), tolerance = 1e-12)
  expect_equal(r$lr_statistic, 2 * (r$loglik - r$loglik_no_selection), tolerance = 1e-12)
  ests <- vapply(c(1, 0.5, 0.1), function(b) morie_bnsadt_median_unbiased(r$target, r$target_se, params = b), 0)
  expect_equal(c(r$bound_lower, r$bound_upper), range(ests), tolerance = 1e-12)
  nf <- morie_bnsadt(bn_x, bn_s, fit = FALSE, target = 1, target_se = 0.5, grid = 1)
  expect_equal(nf$bound_lower, 1, tolerance = 1e-8)
  expect_error(morie_bnsadt(bn_x, bn_s[-1]), "same length")
  expect_error(morie_bnsadt(bn_x, bn_s, target = 1), "target_se is required")
  expect_match(morie_bnsadt_cheatsheet(), "Andrews-Kasy")
})
