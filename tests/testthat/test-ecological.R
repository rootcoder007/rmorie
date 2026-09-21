# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P12: the decomposition must agree with research/lean/P12Ecological.lean.

test_that("covariance and variance decompose exactly; zero within covariance bounds the individual correlation (cov_decomp, var_decomp, ecological_ge)", {
  set.seed(1)
  for (k in 1:100) {
    n <- 200; g <- sample(letters[1:5], n, replace = TRUE)
    x <- rnorm(n) + as.numeric(factor(g)); y <- rnorm(n) - as.numeric(factor(g))
    d <- morie_ecological_decompose(x, y, g)
    expect_equal(unname(d$cov["individual"]), unname(d$cov["between"] + d$cov["within"]), tolerance = 1e-12)
    expect_equal(unname(d$var_x["individual"]), unname(d$var_x["between"] + d$var_x["within"]), tolerance = 1e-12)
    expect_gte(unname(d$var_x["within"]), -1e-12)
  }
  # construct zero within covariance exactly: y within part orthogonal to x within part
  n <- 60; g <- rep(c("a", "b", "c"), each = 20)
  x <- rnorm(n); bx <- stats::ave(x, g); wx <- x - bx
  # y = group effect + a within part orthogonal to wx within each group
  wy <- stats::ave(seq_len(n), g, FUN = function(i) { v <- rnorm(length(i)); v <- v - mean(v); v - sum(v * wx[i]) / sum(wx[i]^2) * wx[i] })
  y <- c(a = 5, b = 1, c = 3)[g] + wy
  d <- morie_ecological_decompose(x, y, g)
  expect_true(d$bound_applies)
  expect_lte(d$corr_individual^2, d$corr_ecological^2 + 1e-10)
})

test_that("Robinson's reversal: the ecological sign can be opposite to the individual sign", {
  x <- c(0, 2, 1, 3); y <- c(1, 3, 0, 2); g <- c("a", "a", "b", "b")
  d <- morie_ecological_decompose(x, y, g)
  expect_equal(d$corr_individual, 0.6, tolerance = 1e-12)
  expect_equal(d$corr_ecological, -1, tolerance = 1e-12)
  expect_true(d$sign_reversed); expect_false(d$bound_applies)
  expect_error(morie_ecological_decompose(1:3, 1:2, c("a", "a", "b")), "equal length")
})
