# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P3, P5, P11, P13: every argument check refuses in words, and the branches the main tests leave out.

test_that("meta pooling refuses bad input and the simulation checks its arguments", {
  expect_error(morie_meta_random_effects(c(1, 2), c(1, NA)), "positive")
  expect_error(morie_meta_random_effects(c(1, 2), c(1, 1), level = 1), "level")
  expect_error(morie_meta_dl_bias(c(1, 2), n_draws = 0), "n_draws")
  expect_error(morie_meta_dl_bias(c(1, 2), seed = -1), "seed")
  expect_error(morie_meta_dl_bias(1), "at least two")
  expect_error(morie_meta_random_effects(c(1, 2), 1), "equal length")
})

test_that("Imbens-Manski refuses bad input", {
  expect_error(morie_bounds_confidence("a", 1, 1, 1), "single numbers")
  expect_error(morie_bounds_confidence(0, 1, 1, 1, level = 1), "level")
})

test_that("Cheeger refuses bad graphs and the exhaustive search has a size limit", {
  expect_error(morie_cheeger_bound(matrix(c(0, 1, 2, 0), 2), 1), "symmetric")
  expect_error(morie_cheeger_bound(matrix(0, 2, 2), 1), "positive degree")
  A <- matrix(1, 17, 17); diag(A) <- 0
  expect_error(morie_cheeger_bound(A, 1, exhaustive = TRUE), "16 places")
  A3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3)
  expect_equal(morie_cheeger_bound(A3, c(TRUE, FALSE, FALSE))$cut, 2)
})

test_that("separation refuses bad input and the two routes answer on the same data", {
  x <- cbind(a = c(0.1, 0.2, 0.15, 0.25)); y <- c(1, 0, 0, 1)
  expect_error(morie_logit_separation(y[1:3], x), "one entry per row")
  expect_error(morie_logit_separation(c(2, 0, 1, 0), x), "0/1")
  expect_equal(morie_logit_separation(y, x, method = "glm")$separation, "none")
  expect_equal(morie_logit_separation(y, x, intercept = FALSE)$separation, "none")
  skip_if_not_installed("lpSolve")
  r <- morie_logit_separation(y, x, method = "lp")
  expect_equal(r$separation, "none"); expect_true(is.na(r$n_zero_margin))
})
