# Coverage tests for R/deepar_native.R (Salinas et al. 2020): the scale
# factor, Gaussian and negative-binomial likelihoods, the ridge AR fit,
# ancestral sampling on the documented .ghc stream, and the forecast.

dp_z <- c(3, 5, 4, 6, 8, 7, 9, 11, 10, 12, 14, 13)

test_that("scale factor and the two likelihoods", {
  expect_equal(morie_deepar_scale_factor(dp_z), 1 + mean(dp_z))
  expect_equal(morie_deepar_scale_factor(dp_z, t0 = 4), 1 + mean(dp_z[1:4]))
  expect_error(morie_deepar_scale_factor(dp_z, t0 = 0), "at least one")
  expect_equal(morie_deepar_gaussian_loglik(c(1, 2.5), 2, 0.7), dnorm(c(1, 2.5), 2, 0.7, log = TRUE), tolerance = 1e-12)
  expect_equal(morie_deepar_gaussian_loglik(1, 1, 0), -log(1e-12) - 0.5 * log(2 * pi))
  expect_equal(morie_deepar_negative_binomial_loglik(4, 3.2, 0.4), dnbinom(4, size = 2.5, mu = 3.2, log = TRUE), tolerance = 1e-12)
  expect_equal(morie_deepar_negative_binomial_loglik(4, 3.2, 0), dpois(4, 3.2, log = TRUE), tolerance = 1e-12)
  expect_error(morie_deepar_negative_binomial_loglik(-1, 3, 0.1), "non-negative count")
  expect_error(morie_deepar_negative_binomial_loglik(1, 3, -0.1), "alpha must be non-negative")
})

test_that("the fit is a ridge AR on the scaled series", {
  nu <- 1 + mean(dp_z)
  s <- dp_z / nu
  y <- s[3:12]
  X <- cbind(1, s[2:11], s[1:10])
  f <- morie_deepar_fit(dp_z, ridge = 0)
  expect_equal(f$nu, nu)
  expect_equal(f$beta, unname(coef(lm(y ~ X - 1))), tolerance = 1e-9)
  fit <- pmax(as.numeric(X %*% f$beta), 0)
  expect_equal(f$fitted_scaled, fit, tolerance = 1e-12)
  expect_equal(f$alpha, max((var(y) - mean(fit)) / mean(fit)^2, 1e-8), tolerance = 1e-12)
  r <- morie_deepar_fit(dp_z, ridge = 0.5)
  expect_equal(r$beta, as.numeric(solve(crossprod(X) + 0.5 * diag(3), crossprod(X, y))), tolerance = 1e-12)
  g <- morie_deepar_fit(dp_z, n_lags = 1, likelihood = "gaussian")
  expect_equal(g$alpha, sd(g$residual))
  expect_error(morie_deepar_fit(dp_z, likelihood = "poisson"), "negative-binomial or gaussian")
  expect_error(morie_deepar_fit(dp_z[1:5], n_lags = 2), "too few")
})

test_that("Gaussian ancestral sampling follows the stream on the real scale", {
  f <- morie_deepar_fit(dp_z, likelihood = "gaussian")
  p <- morie_deepar_sample(f, dp_z, 3, n_samples = 2, seed = 5)
  e <- .ghc_rng(5L)
  for (s in 1:2) {
    st <- dp_z[11:12] / f$nu
    for (h in 1:3) {
      mu <- max(f$beta[1] + f$beta[2] * st[length(st)] + f$beta[3] * st[length(st) - 1], 0)
      d <- mu + f$alpha * .ghc_norm(e, 1L)
      st <- c(st, d)
      expect_equal(p[[s]][h], d * f$nu, tolerance = 1e-12)
    }
  }
  expect_error(morie_deepar_sample(f, 5, 2), "at least 2 history points")
})

test_that("Poisson and negative-binomial draws", {
  pf <- list(beta = c(2, 0.5), n_lags = 1L, nu = 1, alpha = 0, likelihood = "negative-binomial")
  p <- morie_deepar_sample(pf, 3, 2, n_samples = 3, seed = 1)
  e <- .ghc_rng(1L)
  for (s in 1:3) {
    st <- 3
    for (h in 1:2) {
      lam <- 2 + 0.5 * st
      pr <- 1
      k <- 0
      repeat {
        pr <- pr * .ghc_unif(e, 1L)
        if (pr <= exp(-lam)) break
        k <- k + 1
      }
      st <- k
      expect_equal(p[[s]][h], k)
    }
  }
  big <- morie_deepar_sample(list(beta = c(40, 0), n_lags = 1L, nu = 1, alpha = 0, likelihood = "negative-binomial"), 1, 1, n_samples = 2, seed = 3)
  e <- .ghc_rng(3L)
  expect_equal(unlist(big), floor(40 + sqrt(40) * .ghc_norm(e, 2L) + 0.5))
  # statistical check of the gamma-Poisson mixture: mean mu, variance
  # mu + alpha mu^2; 4000 draws, tolerances are about 4 standard errors
  for (a in c(0.5, 2)) {
    d <- unlist(morie_deepar_sample(list(beta = c(5, 0), n_lags = 1L, nu = 1, alpha = a, likelihood = "negative-binomial"), 1, 1, n_samples = 4000, seed = 11))
    v <- 5 + a * 25
    expect_equal(mean(d), 5, tolerance = 4 * sqrt(v / 4000) / 5)
    expect_equal(var(d), v, tolerance = 0.25)
  }
})

test_that("forecast quantiles and interval width come from the sample paths", {
  r <- morie_deepar_forecast(dp_z, 3, likelihood = "gaussian", n_samples = 50, seed = 2)
  p <- morie_deepar_sample(morie_deepar_fit(dp_z, likelihood = "gaussian"), dp_z, 3, n_samples = 50, seed = 2)
  P <- do.call(rbind, p)
  expect_equal(r$mean, colMeans(P), tolerance = 1e-12)
  expect_equal(r$quantiles[["0.1"]], apply(P, 2, quantile, 0.1, names = FALSE), tolerance = 1e-12)
  expect_equal(r$width, r$quantiles[["0.9"]] - r$quantiles[["0.1"]])
  expect_identical(morie_deepar, morie_deepar_forecast)
  expect_error(morie_deepar_forecast(dp_z, 2, quantiles = c(0.5, 1), n_samples = 5), "in \\(0, 1\\)")
})
