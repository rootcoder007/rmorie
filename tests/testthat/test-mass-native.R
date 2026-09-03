# SPDX-License-Identifier: AGPL-3.0-or-later
# Structural tests for native MASS utilities (module 30). No MASS needed.

test_that(".morie_ginv satisfies the Moore-Penrose conditions", {
  set.seed(1)
  A <- matrix(rnorm(20), 5, 4)
  G <- rmorie:::.morie_ginv(A)
  expect_equal(dim(G), c(4L, 5L))
  expect_equal(A %*% G %*% A, A, tolerance = 1e-8)          # AGA = A
  expect_equal(G %*% A %*% G, G, tolerance = 1e-8)          # GAG = G
  expect_equal(t(A %*% G), A %*% G, tolerance = 1e-8)       # (AG)' = AG
  # inverse of a full-rank square matrix equals solve()
  B <- crossprod(matrix(rnorm(9), 3))
  expect_equal(rmorie:::.morie_ginv(B), solve(B), tolerance = 1e-8)
})

test_that("morie_mvrnorm returns the right shape and recovers moments", {
  mu <- c(2, -1)
  Sig <- matrix(c(1, 0.5, 0.5, 2), 2)
  x1 <- morie_mvrnorm(1, mu, Sig)
  expect_length(x1, 2L)
  X <- morie_mvrnorm(5000, mu, Sig)
  expect_equal(dim(X), c(5000L, 2L))
  expect_equal(colMeans(X), mu, tolerance = 0.1)
  expect_equal(cov(X), Sig, tolerance = 0.15)
  expect_error(morie_mvrnorm(1, mu, matrix(c(1, 2, 2, 1), 2)),
               "not positive definite")
})

# --- Module 31: structural (no MASS reference needed) ----------------

test_that("morie_glm_nb recovers a known NB relationship", {
  set.seed(1)
  n <- 300
  x <- rnorm(n)
  y <- rnbinom(n, mu = exp(0.3 + 0.9 * x), size = 3)
  fit <- suppressWarnings(morie_glm_nb(y ~ x, data = data.frame(y, x)))
  expect_s3_class(fit, "negbin")
  expect_equal(unname(coef(fit)["x"]), 0.9, tolerance = 0.15)
  expect_true(fit$theta > 1 && fit$theta < 8)
  expect_true(is.finite(as.numeric(logLik(fit))))
  expect_true(is.finite(AIC(fit)))
  sm <- summary(fit)$coefficients
  expect_true(all(c("Estimate", "Std. Error") %in% colnames(sm)))
})

test_that("morie_kde2d returns a proper grid density", {
  set.seed(2)
  x <- rnorm(80)
  y <- rnorm(80)
  k <- morie_kde2d(x, y, n = 20)
  expect_equal(dim(k$z), c(20L, 20L))
  expect_length(k$x, 20L)
  expect_length(k$y, 20L)
  expect_true(all(k$z >= 0))
  expect_error(morie_kde2d(1:3, 1:4), "same length")
  expect_error(morie_kde2d(x, y, h = -1), "strictly positive")
})

test_that("morie_rlm downweights outliers vs OLS", {
  set.seed(3)
  n <- 100
  x <- rnorm(n)
  y <- 2 * x + rnorm(n)
  y[c(1, 2, 3)] <- y[c(1, 2, 3)] + 40
  rob <- morie_rlm(y ~ x, data = data.frame(y, x))
  ols <- coef(lm(y ~ x, data = data.frame(y, x)))["x"]
  expect_s3_class(rob, "morie_rlm")
  expect_lt(abs(rob$coefficients["x"] - 2), abs(ols - 2))
  sm <- summary(rob)$coefficients
  expect_true(all(c("Value", "Std. Error", "t value") %in% colnames(sm)))
})

test_that("morie_polr fits an ordered factor and yields a valid logLik", {
  set.seed(4)
  n <- 250
  x <- rnorm(n)
  yc <- 1 + (runif(n) > plogis(-0.5 - x)) + (runif(n) > plogis(1 - x))
  yf <- factor(pmin(yc, 3), levels = 1:3, ordered = TRUE)
  fit <- morie_polr(yf ~ x, data = data.frame(yf, x))
  expect_s3_class(fit, "morie_polr")
  expect_length(fit$zeta, 2L)
  expect_true(all(diff(fit$zeta) > 0))       # cutpoints ordered
  expect_true(is.finite(as.numeric(logLik(fit))))
  expect_error(morie_polr(factor(rep(1:2, n / 2), ordered = TRUE) ~ x,
                          data = data.frame(x)), "3 or more levels")
})
