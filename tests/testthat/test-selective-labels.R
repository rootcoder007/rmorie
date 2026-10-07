# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P18: selective labels must agree with research/lean/P18Selective.lean.

test_that("contraction identifies a nested rule; otherwise the interval has the unobserved share as width and is attained", {
  set.seed(19)
  for (k in 1:40) {
    n <- 300; risk <- runif(n); y <- rbinom(n, 1, risk); w <- runif(n, 0.5, 2)
    released <- risk < 0.7
    inner <- risk < runif(1, 0.2, 0.7)
    r <- morie_selective_labels(y, released, inner, w)
    expect_true(r$identified)
    expect_equal(r$rule_rate, sum(w[inner] * y[inner]) / sum(w[inner]), tolerance = 1e-12)   # nested_rate_identified
    expect_equal(r$width, 0)
    expect_equal(r$observed_rate, sum(w[released] * y[released]) / sum(w[released]), tolerance = 1e-12)  # observed_rate_is_conditional
    outer <- risk < runif(1, 0.75, 1)
    r2 <- morie_selective_labels(y, released, outer, w)
    expect_false(r2$identified)
    expect_equal(r2$width, sum(w[outer & !released]) / sum(w[outer]), tolerance = 1e-12)           # unobserved_width
    # every filling of the unobserved outcomes gives a rate inside the interval; the extremes attain the ends
    for (fill in list(rep(0, n), rep(1, n), rbinom(n, 1, 0.5))) {
      yy <- ifelse(released, y, fill)
      rate <- sum(w[outer] * yy[outer]) / sum(w[outer])
      expect_gte(rate, r2$bounds[["lower"]] - 1e-12); expect_lte(rate, r2$bounds[["upper"]] + 1e-12)   # unobserved_bounds
    }
    y0 <- ifelse(released, y, 0); y1 <- ifelse(released, y, 1)
    expect_equal(sum(w[outer] * y0[outer]) / sum(w[outer]), r2$bounds[["lower"]], tolerance = 1e-12)  # unobserved_ends_attained
    expect_equal(sum(w[outer] * y1[outer]) / sum(w[outer]), r2$bounds[["upper"]], tolerance = 1e-12)
    expect_gte(r2$naive_rate, 0); expect_lte(r2$naive_rate, 1)
  }
  expect_error(morie_selective_labels(c(0, 1), c(TRUE, TRUE), c(TRUE)), "equal length")
  expect_error(morie_selective_labels(c(0, 1), c(TRUE, NA), c(TRUE, TRUE)), "NA")
  expect_error(morie_selective_labels(c(0, 2), c(TRUE, TRUE), c(TRUE, TRUE)), "0/1")
  expect_error(morie_selective_labels(c(0, 1), c(TRUE, TRUE), c(TRUE, TRUE), weights = c(-1, 1)), "non-negative")
  expect_error(morie_selective_labels(c(0, 1), c(FALSE, FALSE), c(TRUE, TRUE)), "positive mass")
})
