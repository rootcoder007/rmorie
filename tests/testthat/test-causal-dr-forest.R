# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("morie_estimate_dr_forest returns a doubly-robust ATE near the truth", {
  # native R-learner forest: no grf needed
  set.seed(42)
  n <- 400
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  w <- stats::rbinom(n, 1, stats::plogis(0.5 * x1))
  y <- 1.0 * w + x1 + 0.5 * x2 + stats::rnorm(n)   # true ATE = 1.0
  df <- data.frame(y = y, w = w, x1 = x1, x2 = x2)

  res <- morie_estimate_dr_forest(df, treatment = "w", outcome = "y",
                                  covariates = c("x1", "x2"))
  expect_type(res, "list")
  expect_true(is.finite(res$ate) && is.finite(res$se) && res$se > 0)
  expect_true(res$ci_lower < res$ci_upper)
  expect_equal(res$n, n)
  expect_true(res$ate > 0.3 && res$ate < 1.7)      # near the true ATE of 1.0
})

test_that("morie_estimate_dr_forest validates degenerate input", {
  # native engine runs without grf; a one-row single-arm frame must
  # still error clearly rather than crash inside the nuisance fits
  expect_error(
    morie_estimate_dr_forest(data.frame(y = 1, w = 0, x = 1), "w", "y", "x")
  )
})
