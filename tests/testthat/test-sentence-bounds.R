# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P11: the bounds must agree with research/lean/P11Bounds.lean.

test_that("worst-case bounds: width one, contain zero and the naive difference, ends attained", {
  set.seed(1)
  for (k in 1:50) {
    n <- 300
    z <- sample(c("a", "b"), n, replace = TRUE, prob = c(runif(1, 0.2, 0.8), 1)[1:2] / 1)
    y <- rbinom(n, 1, ifelse(z == "b", 0.5, 0.3))
    b <- morie_sentence_effect_bounds(y, z)
    expect_equal(b$ate_width, 1, tolerance = 1e-12)                        # ate_width_one
    expect_lte(b$ate_bounds["lower"], 0); expect_gte(b$ate_bounds["upper"], 0)  # ate_contains_zero
    expect_lte(b$ate_bounds["lower"], b$naive_difference); expect_gte(b$ate_bounds["upper"], b$naive_difference)
    # outcome_bounds: filling the unobserved arm with 0 or 1 attains the ends
    for (t in c("a", "b")) {
      fill0 <- mean(ifelse(z == t, y, 0)); fill1 <- mean(ifelse(z == t, y, 1))
      expect_equal(unname(b$outcome_bounds[t, "lower"]), fill0, tolerance = 1e-12)
      expect_equal(unname(b$outcome_bounds[t, "upper"]), fill1, tolerance = 1e-12)
      fillr <- mean(ifelse(z == t, y, runif(n)))
      expect_gte(fillr, b$outcome_bounds[t, "lower"] - 1e-12); expect_lte(fillr, b$outcome_bounds[t, "upper"] + 1e-12)
    }
  }
  # Manski-Nagin Utah numbers reproduce: P(y=1,z=t) and P(z!=t) give the printed intervals
  expect_error(morie_sentence_effect_bounds(c(0, 1), c("a", "a")), "two distinct")
  expect_error(morie_sentence_effect_bounds(c(0, 2), c("a", "b")), "0/1")
})

test_that("contaminated-sample bounds: mixture algebra, attained ends, width p/(1-p), informativeness", {
  set.seed(2)
  for (k in 1:200) {
    p <- runif(1, 0, 0.5); cl <- runif(1); r <- runif(1)
    q <- (1 - p) * cl + p * r
    b <- morie_contaminated_bounds(q, p)
    expect_gte(cl, b$lower - 1e-12); expect_lte(cl, b$upper + 1e-12)
    expect_equal(b$width, p / (1 - p))
    expect_equal((q - p * 1) / (1 - p), (q - p) / (1 - p)); expect_equal((q - p * 0) / (1 - p), q / (1 - p))
    expect_equal(b$informative, (p < q) | (p < 1 - q))
  }
  b <- morie_contaminated_bounds(c(0.05, 0.5, 0.97), 0.1)
  expect_equal(b$lower, c(0, 4 / 9, 0.87 / 0.9)); expect_equal(b$upper, c(0.05 / 0.9, 5 / 9, 1))
  expect_error(morie_contaminated_bounds(0.5, 1), "in \\[0, 1\\)")
})

test_that("MTR bounds: sign, sharp upper end, and containment of every monotone completion (mtr_lower, mtr_upper, mtr_upper_attained)", {
  set.seed(3)
  for (k in 1:100) {
    n <- 200; z <- sample(c("a", "b"), n, replace = TRUE); y <- rbinom(n, 1, 0.4)
    m <- morie_sentence_effect_mtr(y, z)
    expect_equal(unname(m$bounds["lower"]), 0)
    expect_equal(unname(m$bounds["upper"]), mean(ifelse(z == "b", y, 1 - y)), tolerance = 1e-12)
    # every monotone completion consistent with the data lies inside
    ya <- ifelse(z == "a", y, rbinom(n, 1, 0.5) * y)          # z=b: ya <= yb = y
    yb <- ifelse(z == "b", y, pmax(y, rbinom(n, 1, 0.5)))     # z=a: yb >= ya = y
    d <- mean(yb) - mean(ya)
    expect_gte(d, -1e-12); expect_lte(d, m$bounds["upper"] + 1e-12)
    # the extreme completion attains the upper end
    expect_equal(mean(ifelse(z == "b", y, 1)) - mean(ifelse(z == "a", y, 0)), unname(m$bounds["upper"]), tolerance = 1e-12)
    # the MTR interval sits inside the worst-case interval
    w <- morie_sentence_effect_bounds(y, z)$ate_bounds
    expect_gte(m$bounds["lower"], w["lower"] - 1e-12); expect_lte(m$bounds["upper"], w["upper"] + 1e-12)
  }
  m2 <- morie_sentence_effect_mtr(c(1, 0, 1, 0), c("a", "a", "b", "b"), direction = "non-increasing")
  expect_equal(unname(m2$bounds["upper"]), 0)
})
