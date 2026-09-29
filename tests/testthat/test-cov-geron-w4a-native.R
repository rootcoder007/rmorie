# Coverage tests for the untested helpers of R/geron_w4a_native.R (Geron,
# Hands-On ML): diffusion beta schedules, replay-buffer checks, dueling
# Q values, causal masks, Gaussian log densities, Inception parameter
# counts, the LCG Box-Muller sampler and ridge regression.

test_that("diffusion beta schedules", {
  expect_equal(morie_beta_schedule_values(5), seq(1e-4, 0.02, length.out = 5))
  T <- 10
  f <- cos(((0:T) / T + 0.008) / 1.008 * pi / 2)^2
  ab <- f / f[1]
  expect_equal(morie_beta_schedule_values(T, "cosine"), pmin(pmax(1 - ab[-1] / ab[-(T + 1)], 1e-8), 0.999), tolerance = 1e-12)
  expect_equal(morie_beta_schedule_values(3, c(0.1, 0.2, 0.3)), c(0.1, 0.2, 0.3))
  expect_error(morie_beta_schedule_values(3, c(0.1, 0.2)), "wrong length")
  expect_error(morie_beta_schedule_values(3, "sigmoid"), "linear")
})

test_that("replay buffer validation", {
  b <- list(list(0, 1, 0.5, 2), list(2, 0, -1, 1, TRUE))
  r <- morie_check_buffer(b, 3, 2, "dqn")
  expect_equal(r$s, c(0L, 2L))
  expect_equal(r$r, c(0.5, -1))
  expect_equal(r$done, c(FALSE, TRUE))
  expect_error(morie_check_buffer(list(), 3, 2, "dqn"), "buffer is empty")
  expect_error(morie_check_buffer(list(list(0, 1, 0, 3)), 3, 2, "dqn"), "state index")
  expect_error(morie_check_buffer(list(list(0, 2, 0, 1)), 3, 2, "dqn"), "action index")
  expect_error(morie_check_buffer(list(list(0, 1, 0)), 3, 2, "dqn"), "4 or 5 fields")
})

test_that("dueling Q = V + A - mean(A), causal masks, Gaussian log density", {
  A <- rbind(c(1, 2, 3), c(-1, 0, 4))
  expect_equal(morie_dueling_q(c(0.5, 2), A), c(0.5, 2) + A - rowMeans(A))
  expect_error(morie_dueling_q(1:3, A), "row mismatch")
  m <- morie_geron_causal_mask(4)
  expect_equal(m, upper.tri(matrix(0, 4, 4)))
  expect_equal(morie_geron_causal_mask(1), matrix(FALSE, 1, 1))
  expect_error(morie_geron_causal_mask(0), ">= 1")
  X <- rbind(c(0.2, -0.1), c(1, 0.5), c(-0.7, 0.9))
  S <- rbind(c(1.5, 0.3), c(0.3, 0.8))
  mu <- c(0.1, 0.2)
  d <- sweep(X, 2, mu)
  ref <- -0.5 * (2 * log(2 * pi) + log(det(S)) + rowSums((d %*% solve(S)) * d))
  expect_equal(morie_gmm_log_pdf(X, mu, S), ref, tolerance = 1e-12)
  expect_error(morie_gmm_log_pdf(X, mu, rbind(c(1, 2), c(2, 1))))
})

test_that("Inception module parameter counts", {
  r <- morie_inception_module(192, 64, 96, 128, 16, 32, 32)
  expect_equal(r$branch_1x1, 192 * 64 + 64)
  expect_equal(r$branch_3x3, 192 * 96 + 96 + 9 * 96 * 128 + 128)
  expect_equal(r$branch_5x5, 192 * 16 + 16 + 25 * 16 * 32 + 32)
  expect_equal(r$out_channels, 256)
  expect_equal(r$params, r$branch_1x1 + r$branch_3x3 + r$branch_5x5 + r$branch_pool)
  expect_equal(r$reduction_saving, 25 * 192 * 32 + 32 - r$branch_5x5)
})

test_that("LCG Box-Muller normals and ridge regression", {
  s <- 7
  u <- numeric(6)
  for (i in 1:6) {
    s <- (1664525 * s + 1013904223) %% 2^32
    u[i] <- (s + 0.5) / 2^32
  }
  r <- sqrt(-2 * log(u[c(1, 3, 5)]))
  z <- c(rbind(r * cos(2 * pi * u[c(2, 4, 6)]), r * sin(2 * pi * u[c(2, 4, 6)])))
  expect_equal(morie_lcg_normal(5, 7), z[1:5], tolerance = 1e-12)
  a <- morie_lcg_normal(c(2, 3), 7)
  expect_equal(a, matrix(z, 2, 3, byrow = TRUE), tolerance = 1e-12)
  X <- cbind(1, c(0.2, 1.1, 1.9, 3.2, 4.1))
  y <- c(1.1, 2.9, 4.2, 7.1, 8.8)
  f <- morie_ridge_estimator(X, y, alpha = 0.5)
  th <- solve(crossprod(X) + 0.5 * diag(2), crossprod(X, y))
  Xn <- cbind(1, c(0.5, 2.5))
  expect_equal(f(Xn), as.numeric(Xn %*% th), tolerance = 1e-12)
  expect_equal(morie_ridge_estimator(X, y)(X), as.numeric(fitted(lm(y ~ X - 1))), tolerance = 1e-10)
})
