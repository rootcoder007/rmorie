# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P5: separation detection must agree with research/lean/P5Separation.lean.

loglik_of <- function(y, x, b) {
  s <- 2 * y - 1
  sum(-log1p(exp(-as.numeric((s * cbind(1, x)) %*% b))))
}

test_that("a perfectly predictive dummy gives complete separation: the likelihood climbs to 0 along d and never attains it", {
  x <- cbind(z = c(1, 1, 1, 0, 0, 0, 0), w = c(0.2, -1, 0.5, 0.1, -0.4, 1.2, 0.3))
  y <- c(1, 1, 1, 0, 0, 0, 0)
  r <- morie_logit_separation(y, x)
  expect_equal(r$separation, "complete")
  expect_equal(r$method, "lp")
  expect_true(all(r$margins > 0))                                     # CompleteSeparation
  ll <- r$loglik_along
  expect_true(all(diff(ll) > 0))                                      # loglik_lt_shift
  expect_true(all(ll < 0))                                            # loglik_neg
  expect_gt(ll[["t=100"]], -1e-6)                                     # loglik_tendsto_zero
  # from any start b, b + d is strictly better (no_mle)
  set.seed(1)
  for (k in 1:20) {
    b <- rnorm(3)
    expect_gt(loglik_of(y, x, b + r$direction), loglik_of(y, x, b))
  }
})

test_that("quasi-complete separation is reported when one margin can only be zero", {
  # y = 1 whenever z = 1, but some y = 0 also have z = 1: the z = 1, y = 0 rows pin the margin at zero
  x <- cbind(z = c(1, 1, 1, 1, 0, 0, 0), w = c(0.3, -0.2, 0.9, 0.1, -0.5, 0.4, 1.1))
  y <- c(1, 1, 1, 0, 0, 0, 0)
  r <- morie_logit_separation(y, x)
  expect_true(r$separation %in% c("quasi-complete", "complete"))
  expect_true(all(r$margins > -1e-8))
  expect_true(any(r$margins > 1e-8))
  set.seed(2)
  for (k in 1:20) {
    b <- rnorm(3)
    expect_gt(loglik_of(y, x, b + r$direction), loglik_of(y, x, b))  # no_mle_quasi
  }
})

test_that("overlapping data has no separating direction and the fit converges", {
  set.seed(4)
  n <- 200
  x <- cbind(a = rnorm(n), b = rnorm(n))
  y <- rbinom(n, 1, plogis(0.5 * x[, 1] - 0.3 * x[, 2]))
  r <- morie_logit_separation(y, x)
  expect_equal(r$separation, "none")
  expect_null(r$direction)
  fit <- suppressWarnings(stats::glm(y ~ x, family = stats::binomial()))
  expect_true(fit$converged)
  expect_lt(max(abs(coef(fit))), 10)
  # the glm heuristic agrees on both verdicts
  expect_equal(morie_logit_separation(y, x, method = "glm")$separation, "none")
  x2 <- cbind(z = c(1, 1, 1, 0, 0, 0, 0), w = c(0.2, -1, 0.5, 0.1, -0.4, 1.2, 0.3))
  y2 <- c(1, 1, 1, 0, 0, 0, 0)
  expect_true(morie_logit_separation(y2, x2, method = "glm")$separation %in% c("complete", "quasi-complete"))
})
