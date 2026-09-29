# Coverage tests for the untested helpers of R/gp_ch13_15.R: the
# functional-regression coefficient curve and BIC, the zero-truncated
# Poisson MLE, and the negative-binomial and time-varying MSMs.

test_that("functional coefficient curve and BIC", {
  tt <- seq(0, 2, by = 0.25)
  b <- c(0.5, -1, 2, 0.3)
  Psi <- cbind(1, sin(2 * pi * tt / 2), cos(2 * pi * tt / 2), sin(4 * pi * tt / 2))
  expect_equal(morie_fda_beta_function(tt, b, 4), as.numeric(Psi %*% b), tolerance = 1e-12)
  u <- tt / 2
  expect_equal(morie_fda_beta_function(tt, b[1:3], 3, "poly"), b[1] + b[2] * u + b[3] * u^2, tolerance = 1e-12)
  expect_error(morie_fda_beta_function(tt, b, 4, "spline"), "unknown basis")
  expect_equal(morie_fda_bic(-12.5, 4, 30), 25 + 5 * log(30))
})

test_that("zero-truncated Poisson MLE solves mu / (1 - exp(-mu)) = mean", {
  y <- c(1, 2, 1, 3, 4, 1, 2, 5)
  mu <- morie_zap_mle(y)
  ref <- uniroot(function(m) m / (1 - exp(-m)) - mean(y), c(1e-6, 20), tol = 1e-14)$root
  expect_equal(mu, ref, tolerance = 1e-10)
  opt <- optimize(function(m) morie_zap_loglik(y, m), c(0.1, 10), maximum = TRUE, tol = 1e-10)$maximum
  expect_equal(mu, opt, tolerance = 1e-6)
  expect_equal(morie_zap_loglik(y), sum(dpois(y, mu, log = TRUE) - log1p(-exp(-mu))), tolerance = 1e-12)
  expect_equal(morie_zap_mle(c(1, 1, 1)), 0)
  expect_error(morie_zap_mle(numeric(0)), "at least one positive")
})

test_that("negative-binomial MSM reuses the weighted Poisson fit", {
  A <- rbind(c(0, 1, 1), c(1, 1, 1), c(0, 0, 0), c(1, 0, 1), c(0, 0, 1), c(1, 1, 0), c(0, 1, 0), c(1, 1, 1))
  y <- c(2, 6, 1, 3, 2, 5, 1, 7)
  w <- c(1.2, 0.8, 1, 1.5, 0.9, 1.1, 1, 0.7)
  off <- log(c(1, 2, 1, 1.5, 1, 2, 1, 2.5))
  ab <- rowSums(A)
  r <- morie_msm_negative_binomial(y, A, alpha = 0.3, offset = off, weights = w)
  g <- glm(y ~ ab + offset(off), family = poisson, weights = w, control = glm.control(epsilon = 1e-14))
  expect_equal(r$beta, unname(coef(g)), tolerance = 1e-8)
  expect_equal(r$rate_ratio, exp(r$beta[2]))
  expect_equal(r$variance, r$fitted + 0.3 * r$fitted^2, tolerance = 1e-12)
  expect_equal(r$fitted, unname(fitted(g)), tolerance = 1e-8)
  expect_equal(r$alpha, 0.3)
})

test_that("time-varying exposure MSM is weighted least squares on cumulative exposure", {
  A <- list(c(0, 1, 1), c(1, 1, 1), c(0, 0, 0), c(1, 0, 1), c(0, 0, 1), c(1, 1, 0))
  y <- c(2.1, 3.4, 0.2, 2.2, 1.1, 2.5)
  w <- c(1, 2, 0.5, 1.5, 1, 0.8)
  ab <- vapply(A, sum, 0)
  r <- morie_msm_time_varying_exposure(y, A, w)
  expect_equal(r$beta, unname(coef(lm(y ~ ab, weights = w))), tolerance = 1e-10)
  expect_equal(r$estimate, r$beta[2])
  expect_equal(r$a_bar, ab)
  expect_equal(c(r$weight_mean, r$weight_max), c(mean(w), 2))
  u <- morie_msm_time_varying_exposure(y, A)
  expect_equal(u$beta, unname(coef(lm(y ~ ab))), tolerance = 1e-10)
  expect_equal(u$weight_max, 1)
})
