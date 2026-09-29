# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/timesfm_native.R (TimesFM, Das, Kong, Sen & Zhou 2024).
# Left padding to whole patches, the lower-triangular causal mask,
# ceil(H / q) rollout steps and the autoregressive rollout are
# recomputed directly.

test_that("input_patches pads on the left to whole patches", {
  r <- morie_timesfm_input_patches(1:7, 3, pad_value = -1)
  expect_equal(r$patches, list(c(-1, -1, 1), 2:4, 5:7))
  expect_equal(r$n_padded, 2)
  expect_equal(r$L, 7L)
  expect_equal(morie_timesfm_input_patches(list(1, 2, 3, 4), 2)$patches, list(1:2, 3:4))
  expect_error(morie_timesfm_input_patches(1:3, 0), "at least 1")
  expect_error(morie_timesfm_input_patches(numeric(0), 2), "empty")
})

test_that("causal_mask is lower triangular", {
  m <- morie_timesfm_causal_mask(4)
  expect_equal(m$mask, (row(diag(4)) >= col(diag(4))) * 1)
  expect_equal(m$training_signals, 4L)
  expect_error(morie_timesfm_causal_mask(0), "at least one patch")
})

test_that("rollout_steps and horizon_plan count ceil(H / q) steps", {
  expect_equal(morie_timesfm_rollout_steps(10, 3)$steps, 4L)
  expect_true(morie_timesfm_rollout_steps(10, 12)$single_step)
  expect_error(morie_timesfm_rollout_steps(0, 3), "horizon")
  expect_error(morie_timesfm_rollout_steps(3, 0), "output_patch_len")
  hp <- morie_timesfm_horizon_plan(128, 32, 128)
  expect_equal(c(hp$steps_asymmetric, hp$steps_symmetric, hp$steps_direct), c(1L, 4L, 1L))
  expect_equal(hp$speedup_vs_symmetric, 4)
})

test_that("morie_timesfm rolls the predictor forward on its own output", {
  # predictor: next q values continue the last patch's slope
  pred <- function(pat) {
    last <- pat[[length(pat)]]
    s <- last[length(last)] - last[length(last) - 1]
    last[length(last)] + s * (1:3)
  }
  r <- morie_timesfm(c(2, 4, 6, 8), pred, horizon = 7, input_patch_len = 2, output_patch_len = 3)
  expect_equal(r$forecast, seq(10, 22, by = 2))
  expect_equal(r$steps, 3L)
  expect_equal(r$context_grew_to, 4L + 9L)
  seen <- list()
  spy <- function(pat) {
    seen[[length(seen) + 1L]] <<- pat
    c(0, 0, 0)
  }
  morie_timesfm(1:5, spy, 4, 2, 3)
  expect_equal(seen[[2]], morie_timesfm_input_patches(c(1:5, 0, 0, 0), 2)$patches)
  expect_error(morie_timesfm(1:4, function(p) 1, 3, 2, 3), "returned 1 values")
})

test_that("morie_timesfm_cheatsheet states the step count", {
  expect_match(morie_timesfm_cheatsheet(), "ceil(H/q)", fixed = TRUE)
})
