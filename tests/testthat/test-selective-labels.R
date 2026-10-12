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

test_that("judges ordered by leniency: the interval narrows along the chain and collapses exactly on containment", {
  set.seed(21)
  for (k in 1:30) {
    n <- 250; risk <- runif(n); y <- rbinom(n, 1, risk); w <- runif(n, 0.5, 2)
    cuts <- sort(runif(4, 0.3, 0.95))
    judges <- sapply(cuts, function(ct) risk < ct)
    rule <- risk < runif(1, 0.2, 1)
    r <- morie_selective_labels_judges(y, judges, rule, w)
    expect_true(r$nested)
    # chain_width_antitone: widths non-increasing from strict to lenient
    expect_true(all(diff(r$judges$width) <= 1e-12))
    # each row is the single-judge computation (unobserved_width)
    for (j in 1:4) {
      expect_equal(r$judges$width[j], sum(w[rule & !judges[, j]]) / sum(w[rule]), tolerance = 1e-12)
      expect_equal(r$judges$lower[j], sum(w[rule & judges[, j]] * y[rule & judges[, j]]) / sum(w[rule]), tolerance = 1e-12)
    }
    # chain_width_zero_iff: width zero exactly when the rule's set lies inside the judge's
    expect_equal(r$judges$width == 0, vapply(1:4, function(j) !any(rule & !judges[, j]), logical(1)))
    # chain_contraction: inside the union of the chain = inside the most lenient judge
    inside_union <- !any(rule & !apply(judges, 1, any))
    expect_equal(r$judges$identified[4], inside_union)
    expect_equal(r$best, r$judges$judge[which.min(r$judges$width)])
    if (r$identified) expect_equal(r$rule_rate, sum(w[rule] * y[rule]) / sum(w[rule]), tolerance = 1e-12)
  }
  # judges that are not nested: reported, and the sharpest interval still chosen
  n <- 100; risk <- runif(n); y <- rbinom(n, 1, risk)
  judges <- list(a = risk < 0.6, b = risk > 0.3)
  r <- morie_selective_labels_judges(y, judges, rule_released = risk < 0.5)
  expect_false(r$nested)
  expect_equal(r$width, min(r$judges$width))
  expect_error(morie_selective_labels_judges(y, judges[[1]][1:10], risk < 0.5), "one row per defendant")
})
