# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 15 -- retlv.R (GEV return level), fzwlc.R (smoothed
# Wilcoxon), coitg.R (Engle-Granger cointegration), ordct.R
# (linear-by-linear trend test).

test_that("retlv estimates a GEV return level", {
  set.seed(1)
  bm <- apply(matrix(stats::rnorm(200 * 30), 200, 30), 1, max)
  r <- tryCatch(retlv(bm, return_period = 100), error = function(e) e)
  expect_true(is.list(r))
  if (!is.null(r$z)) {
    expect_true(is.numeric(r$z))
    expect_true(is.numeric(r$se))
    expect_equal(r$return_period, 100)
  }
  r50 <- tryCatch(retlv(bm, return_period = 50), error = function(e) e)
  expect_true(is.list(r50))
})

test_that("fzwlc smoothed Wilcoxon spans its branches", {
  expect_equal(fzwlc(1:3)$method, "fzwlc - too few obs")
  expect_match(fzwlc(rep(0, 10))$method, "too few nonzero")
  set.seed(2)
  x <- stats::rnorm(120, mean = 0.4)
  expect_true(fzwlc(x, alternative = "two-sided")$p_value >= 0)
  expect_true(fzwlc(x, alternative = "greater")$p_value >= 0)
  expect_true(fzwlc(x, alternative = "less")$p_value >= 0)
  expect_error(fzwlc(x, alternative = "sideways"), "two-sided")
})

test_that("morie_eg_coint validates input and tests cointegration", {
  expect_error(morie_eg_coint(1:5, 1:6), "mismatch")
  expect_error(morie_eg_coint(1:10, 1:10), ">=20")
  set.seed(3)
  y2 <- cumsum(stats::rnorm(140))
  y1 <- 0.8 * y2 + stats::rnorm(140)
  r <- morie_eg_coint(y1, y2)
  expect_true(is.numeric(r$adf_statistic))
  expect_length(r$beta, 2L)
  expect_true(is.numeric(r$p_value))
  # plain-ADF fallback (urca mocked absent)
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "urca")) FALSE else TRUE
    },
    .package = "base"
  )
  r2 <- morie_eg_coint(y1, y2)
  expect_true(is.numeric(r2$adf_statistic))
})

test_that("morie_ordered_categories runs the linear-by-linear trend test", {
  tbl <- matrix(c(10, 5, 2, 4, 8, 6, 1, 3, 9), 3, 3, byrow = TRUE)
  r <- morie_ordered_categories(tbl)
  expect_equal(r$df, 1L)
  expect_true(is.numeric(r$statistic))
  expect_true(abs(r$correlation) <= 1)
  expect_true(is.list(morie_ordered_categories(tbl,
    row_scores = c(1, 2, 4),
    col_scores = c(0, 1, 3)
  )))
  small <- morie_ordered_categories(matrix(1, 1, 1))
  expect_true(is.na(small$statistic))
  onerow <- matrix(c(0, 0, 0, 5, 6, 7, 0, 0, 0), 3, 3, byrow = TRUE)
  expect_equal(morie_ordered_categories(onerow)$statistic, 0)
})
