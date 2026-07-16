# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 27 cross-validation: native robust covariance vs sandwich.
library(testthat)
library(rmorie)

set.seed(270)
n <- 300
d <- data.frame(x1 = rnorm(n), x2 = runif(n),
                g = factor(sample(1:20, n, replace = TRUE)))
d$y <- 1 + 0.8 * d$x1 - 0.5 * d$x2 + rnorm(n, sd = 1 + abs(d$x1))
m <- lm(y ~ x1 + x2, data = d)

test_that("native HC0-HC5 match sandwich::vcovHC", {
  skip_if_not_installed("sandwich")
  for (ty in c("const", "HC0", "HC1", "HC2", "HC3", "HC4")) {
    ref <- sandwich::vcovHC(m, type = ty)
    mine <- morie_vcov_hc(m, ty)
    expect_equal(unname(mine), unname(ref), tolerance = 1e-8,
                 info = ty)
  }
})

test_that("native HAC matches sandwich::NeweyWest", {
  skip_if_not_installed("sandwich")
  ref <- sandwich::NeweyWest(m, lag = 4, prewhite = FALSE,
                             adjust = TRUE)
  mine <- morie_vcov_hac(m, lag = 4, adjust = TRUE)
  expect_equal(unname(mine), unname(ref), tolerance = 1e-8)
})

test_that("native CL matches sandwich::vcovCL one-way", {
  skip_if_not_installed("sandwich")
  ref <- sandwich::vcovCL(m, cluster = d$g, type = "HC1")
  mine <- morie_vcov_cl(m, d$g, "HC1")
  expect_equal(unname(mine), unname(ref), tolerance = 1e-8)
  ref0 <- sandwich::vcovCL(m, cluster = d$g, type = "HC0")
  mine0 <- morie_vcov_cl(m, d$g, "HC0")
  expect_equal(unname(mine0), unname(ref0), tolerance = 1e-8)
})

test_that("native HC on a glm matches sandwich", {
  skip_if_not_installed("sandwich")
  set.seed(271)
  gd <- data.frame(x = rnorm(200))
  gd$y <- rbinom(200, 1, plogis(0.5 * gd$x))
  gm <- glm(y ~ x, data = gd, family = binomial)
  for (ty in c("HC0", "HC1", "HC3")) {
    expect_equal(unname(morie_vcov_hc(gm, ty)),
                 unname(sandwich::vcovHC(gm, type = ty)),
                 tolerance = 1e-7, info = ty)
  }
})
