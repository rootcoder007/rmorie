# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Module 16 cross-validation: native RDD engines vs rdrobust /
# rddensity (reference packages allowed here only).
library(testthat)
library(rmorie)

set.seed(140)
n <- 4000
x <- runif(n, -1, 1)
d <- as.integer(x >= 0)
y <- 0.5 + 0.8 * x - 0.4 * x^2 + 1.2 * d + rnorm(n, 0, 0.4)
df <- data.frame(x = x, y = y)

test_that("native sharp RD point estimate matches rdrobust at fixed h", {
  skip_if_not_installed("rdrobust")
  h <- 0.4
  ref <- rdrobust::rdrobust(y = y, x = x, c = 0, h = h, p = 1,
                            kernel = "triangular")
  mine <- morie_rdd_sharp(df, "y", "x", bandwidth = h)
  # Conventional point estimate is deterministic WLS: exact match.
  expect_equal(mine$estimate, unname(ref$coef["Conventional", 1]),
               tolerance = 1e-8)
  # Conventional NN(3) variance: same estimator family; allow small
  # implementation slack (rdrobust uses its own NN tie-breaking).
  expect_equal(mine$std_error, unname(ref$se["Conventional", 1]),
               tolerance = 0.05)
})

test_that("native kink estimate matches rdrobust deriv=1 at fixed h", {
  skip_if_not_installed("rdrobust")
  set.seed(141)
  yk <- 1 + 0.5 * x + 1.5 * pmax(x, 0) + rnorm(n, 0, 0.2)
  dfk <- data.frame(x = x, y = yk)
  h <- 0.5
  ref <- rdrobust::rdrobust(y = yk, x = x, c = 0, h = h, deriv = 1,
                            p = 2, kernel = "triangular")
  mine <- morie_rdd_kink(dfk, "y", "x", bandwidth = h)
  expect_equal(mine$estimate, unname(ref$coef["Conventional", 1]),
               tolerance = 1e-8)
})

test_that("native IK bandwidth is the same order as rdbwselect mserd", {
  skip_if_not_installed("rdrobust")
  # Asymmetric curvature: IK's bias term is the second-derivative
  # DIFFERENCE at the cutoff, so symmetric DGPs make its bandwidth
  # legitimately large. Compare on an asymmetric design.
  set.seed(143)
  ya <- 0.5 + 0.8 * x - 1.5 * x^2 * (x >= 0) + 0.6 * x^2 * (x < 0) +
    1.2 * d + rnorm(n, 0, 0.4)
  ref <- rdrobust::rdbwselect(y = ya, x = x, c = 0, bwselect = "mserd",
                              kernel = "triangular")
  h_ref <- ref$bws[1, 1]
  h_ik <- morie_rdd_bandwidth_ik(x, ya)$bandwidth
  # IK (2012) and CCT's mserd are distinct refinements of the same
  # MSE-optimal rule: same rate, different plug-in constants.
  expect_gt(h_ik / h_ref, 0.3)
  expect_lt(h_ik / h_ref, 3.5)
})

test_that("native McCrary agrees with rddensity on the verdict", {
  skip_if_not_installed("rddensity")
  set.seed(142)
  x_null <- runif(5000, -1, 1)
  ref <- rddensity::rddensity(x_null, c = 0)
  mine <- morie_rdd_mccrary(x_null)
  expect_gt(ref$test$p_jk, 0.05)
  expect_gt(mine$p_value, 0.05)
  x_manip <- c(runif(4000, -1, 1), runif(1500, 0, 0.06))
  ref2 <- rddensity::rddensity(x_manip, c = 0)
  mine2 <- morie_rdd_mccrary(x_manip)
  expect_lt(ref2$test$p_jk, 0.01)
  expect_lt(mine2$p_value, 0.01)
})
