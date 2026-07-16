# SPDX-License-Identifier: AGPL-3.0-or-later
# Structural tests for native stats primitives (module 26). No reference
# package needed; ppcor/randtests/EValue parity lives in tests/cross/.

test_that("morie_partial_cor equals the precision-matrix formula", {
  set.seed(1); X <- matrix(rnorm(200 * 4), 200, 4)
  X[, 2] <- X[, 2] + 0.6 * X[, 1]
  pc <- morie_partial_cor(X)$estimate
  P <- solve(stats::cor(X))
  ref <- -stats::cov2cor(P); diag(ref) <- 1
  expect_equal(unname(pc), unname(ref), tolerance = 1e-8)
})

test_that("morie_partial_cor_test returns a correlation in [-1,1] and valid p", {
  set.seed(2); n <- 100
  z <- rnorm(n); x <- z + rnorm(n); y <- z + rnorm(n)
  r <- morie_partial_cor_test(x, y, z)
  expect_true(r$estimate >= -1 && r$estimate <= 1)
  expect_true(r$p.value >= 0 && r$p.value <= 1)
})

test_that("morie_semipartial_cor has unit diagonal and right shape", {
  set.seed(3); X <- matrix(rnorm(150 * 3), 150, 3)
  sp <- morie_semipartial_cor(X)$estimate
  expect_equal(dim(sp), c(3L, 3L))
  expect_equal(diag(sp), rep(1, 3), tolerance = 1e-8)
})

test_that("randomness tests return p-values in [0,1]", {
  set.seed(4); x <- rnorm(80)
  for (res in list(morie_runs_test(x), morie_turning_point_test(x),
                   morie_difference_sign_test(x),
                   morie_bartels_rank_test(x))) {
    expect_true(res$p.value >= 0 && res$p.value <= 1)
    expect_true(is.finite(res$statistic))
  }
})

test_that("turning-point test flags a monotone series as non-random", {
  # a strictly increasing series has zero turning points -> small p
  expect_lt(morie_turning_point_test(1:60)$p.value, 0.01)
})

test_that("morie_evalue matches the Ding-VanderWeele closed form", {
  # E = RR + sqrt(RR (RR - 1)) for RR > 1
  rr <- 2.5
  e <- morie_evalue(rr, type = "RR")
  expect_equal(e$point, rr + sqrt(rr * (rr - 1)), tolerance = 1e-8)
  # RR < 1 uses the reciprocal; E >= 1 always
  e2 <- morie_evalue(0.5, type = "RR")
  expect_gte(e2$point, 1)
})
