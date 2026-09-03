# SPDX-License-Identifier: AGPL-3.0-or-later
# Structural tests for native robust covariance (module 27). No reference
# package needed; sandwich parity lives in tests/cross/.

make_lm <- function(n = 60) {
  set.seed(11)
  d <- data.frame(x1 = rnorm(n), x2 = runif(n))
  d$y <- 1 + 0.5 * d$x1 - 0.3 * d$x2 + rnorm(n, sd = 1 + abs(d$x1))
  stats::lm(y ~ x1 + x2, data = d)
}

test_that("morie_vcov_hc is symmetric, named, correct dim for all HC types", {
  m <- make_lm()
  for (ty in c("const", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")) {
    V <- morie_vcov_hc(m, ty)
    expect_equal(dim(V), c(3L, 3L), info = ty)
    expect_equal(V, t(V), tolerance = 1e-10, info = ty)
    expect_equal(rownames(V), names(stats::coef(m)), info = ty)
    expect_true(all(diag(V) > 0), info = ty)
  }
  expect_error(morie_vcov_hc(m, "HCX"), "unknown HC type")
})

test_that("HC1 = HC0 * n/(n-k) and const = classical vcov", {
  m <- make_lm()
  n <- length(stats::residuals(m))
  k <- length(stats::coef(m))
  expect_equal(morie_vcov_hc(m, "HC1"), morie_vcov_hc(m, "HC0") * n / (n - k),
               tolerance = 1e-10)
  expect_equal(unname(morie_vcov_hc(m, "const")), unname(stats::vcov(m)),
               tolerance = 1e-8)
})

test_that("morie_vcov_hac and morie_vcov_cl return valid covariances", {
  m <- make_lm(90)
  H <- morie_vcov_hac(m, lag = 4)
  expect_equal(H, t(H), tolerance = 1e-10)
  expect_true(all(diag(H) > 0))
  cl <- factor(sample(1:12, 90, replace = TRUE))
  for (ty in c("HC0", "HC1")) {
    C <- morie_vcov_cl(m, cl, ty)
    expect_equal(dim(C), c(3L, 3L))
    expect_true(all(diag(C) > 0))
  }
  expect_error(morie_vcov_cl(m, cl, "HC9"), "HC0 or HC1")
})

test_that("morie_vcov_robust dispatches to HC / HAC / CL", {
  m <- make_lm()
  expect_equal(morie_vcov_robust(m, "HC3"), morie_vcov_hc(m, "HC3"))
  expect_equal(morie_vcov_robust(m, "HAC", lag = 3), morie_vcov_hac(m, lag = 3))
  cl <- factor(sample(1:8, length(stats::residuals(m)), replace = TRUE))
  expect_equal(morie_vcov_robust(m, "CL", cluster = cl),
               morie_vcov_cl(m, cl))
  expect_error(morie_vcov_robust(m, "CL"), "needs `cluster`")
})

test_that("glm HC path works and is symmetric PSD", {
  set.seed(12)
  gd <- data.frame(x = rnorm(120))
  gd$y <- rbinom(120, 1, plogis(0.4 * gd$x))
  gm <- stats::glm(y ~ x, data = gd, family = stats::binomial())
  V <- morie_vcov_hc(gm, "HC0")
  expect_equal(dim(V), c(2L, 2L))
  expect_equal(V, t(V), tolerance = 1e-10)
  expect_true(min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) > -1e-8)
})
