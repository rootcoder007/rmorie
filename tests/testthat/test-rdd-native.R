# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 16 — structural tests for the native RDD family.
#' @srrstats {G5.4} Known-DGP recovery for the sharp/fuzzy/kink
#'   estimators; hand-checkable invariants for bandwidth + density.

.mk_rdd <- function(n = 3000, tau = 1.2, seed = 40, fuzzy = FALSE) {
  set.seed(seed)
  x <- runif(n, -1, 1)
  d <- as.integer(x >= 0)
  t_prob <- if (fuzzy) 0.2 + 0.6 * d else d
  tr <- rbinom(n, 1, t_prob)
  y <- 0.5 + 0.8 * x - 0.4 * x^2 + tau * (if (fuzzy) tr else d) +
    rnorm(n, 0, 0.4)
  data.frame(x = x, y = y, tr = tr)
}

test_that("native IK bandwidth is positive, sane, deterministic", {
  df <- .mk_rdd()
  bw <- morie_rdd_bandwidth_ik(df$x, df$y)
  expect_true(is.finite(bw$bandwidth) && bw$bandwidth > 0)
  expect_lt(bw$bandwidth, diff(range(df$x)))
  expect_match(bw$method, "rmorie native")
  bw2 <- morie_rdd_bandwidth_ik(df$x, df$y)
  expect_identical(bw$bandwidth, bw2$bandwidth)
})

test_that("sharp RDD recovers tau with native IK bandwidth", {
  df <- .mk_rdd(tau = 1.2, seed = 41)
  res <- morie_rdd_sharp(df, "y", "x")
  expect_match(res$method, "rmorie native")
  expect_equal(res$estimate, 1.2, tolerance = 0.15)
  expect_true(res$p_value < 0.01)
  # zero effect -> no detection
  df0 <- .mk_rdd(tau = 0, seed = 42)
  res0 <- morie_rdd_sharp(df0, "y", "x")
  expect_lt(abs(res0$estimate), 0.2)
})

test_that("fuzzy RDD recovers the LATE via the Wald ratio", {
  df <- .mk_rdd(tau = 1.5, seed = 43, fuzzy = TRUE)
  res <- morie_rdd_fuzzy(df, "y", "x", "tr")
  expect_equal(res$estimate, 1.5, tolerance = 0.35)
  expect_match(res$method, "rmorie native")
})

test_that("bias-corrected estimate = order-(p+1) fit at rho = 1", {
  df <- .mk_rdd(tau = 1.2, seed = 44)
  bc <- morie_rdd_bias_corrected(df, "y", "x", bandwidth = 0.5, rho = 1)
  ref <- morie_rdd_sharp(df, "y", "x", bandwidth = 0.5, p = 2)
  expect_equal(bc$estimate, ref$estimate, tolerance = 1e-10)
  expect_match(bc$method, "rmorie native")
})

test_that("kink design recovers a slope discontinuity", {
  set.seed(45)
  n <- 4000
  x <- runif(n, -1, 1)
  y <- 1 + 0.5 * x + 1.5 * pmax(x, 0) + rnorm(n, 0, 0.2)
  df <- data.frame(x = x, y = y)
  res <- morie_rdd_kink(df, "y", "x", bandwidth = 0.6)
  expect_equal(res$estimate, 1.5, tolerance = 0.3)
  expect_match(res$method, "rmorie native")
})

test_that("native McCrary: null density passes, manipulated fails", {
  set.seed(46)
  x_null <- runif(5000, -1, 1)
  ok <- morie_rdd_mccrary(x_null)
  expect_match(ok$name, "rmorie native")
  expect_gt(ok$p_value, 0.05)
  # heap mass just right of the cutoff
  x_manip <- c(runif(4000, -1, 1), runif(1200, 0, 0.08))
  bad <- morie_rdd_mccrary(x_manip)
  expect_lt(bad$p_value, 0.01)
  expect_gt(bad$theta, 0)
})
