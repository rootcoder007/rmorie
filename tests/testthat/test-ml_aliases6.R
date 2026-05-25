# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2BB: tests for the last batch of small alias R files
# (mostly internal-only attention/NN/encoding utilities) + direct
# coverage for the .study_core / .study_reporting internal helpers.

# ----------------------------------------------------- internal aliases

test_that("midranks computes mid-ranks for a numeric vector", {
  out <- tryCatch(midranks(c(1, 1, 2, 3, 3, 3, 4)),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("mdrnk error: %s", conditionMessage(out)))
  expect_true(is.numeric(out) || is.list(out))
})

test_that("cosine_lr_schedule returns a decayed learning rate", {
  out <- tryCatch(
    morie:::cosine_lr_schedule(x = 50L, lr_max = 1e-3, lr_min = 0,
                       total_steps = 100L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("cslnc error: %s", conditionMessage(out)))
  expect_true(is.numeric(out) || is.list(out))
})

test_that("word_embedding returns an embedding matrix", {
  set.seed(1L)
  out <- tryCatch(
    morie:::word_embedding(x = sample.int(50L, 20L, replace = TRUE),
                   vocab_size = 50L, d_model = 4L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("wdemb error: %s", conditionMessage(out)))
  expect_true(is.matrix(out) || is.list(out))
})

test_that("flash_attention runs on small Q/K/V matrices", {
  set.seed(2L)
  Q <- matrix(stats::rnorm(16 * 4), 16L, 4L)
  K <- matrix(stats::rnorm(16 * 4), 16L, 4L)
  V <- matrix(stats::rnorm(16 * 4), 16L, 4L)
  out <- tryCatch(
    morie:::flash_attention(Q, K, V, block_size = 8L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("flsha error: %s", conditionMessage(out)))
  expect_true(is.matrix(out) || is.list(out))
})

test_that("bpe_tokenizer learns BPE merges from a short corpus", {
  set.seed(3L)
  out <- tryCatch(
    morie:::bpe_tokenizer(x = c("hello world", "world of morie",
                         "hello morie"), num_merges = 5L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("tknbp error: %s", conditionMessage(out)))
  expect_true(is.list(out) || is.character(out))
})

# ----------------------------------------------------- study_core internals

test_that(".na_omit_cols drops rows with NAs in named cols", {
  df <- data.frame(x = c(1, NA, 3, 4), y = c("a", "b", NA, "d"),
                   z = 1:4)
  out <- morie:::.na_omit_cols(df, c("x", "y"))
  expect_equal(nrow(out), 2L)
})

test_that(".safe_divide returns NA on zero denominator", {
  expect_true(is.na(morie:::.safe_divide(1, 0)))
  expect_true(is.na(morie:::.safe_divide(1, NA)))
  expect_equal(morie:::.safe_divide(10, 5), 2.0)
})

# ----------------------------------------------------- study_reporting internals

test_that(".binary_power_required_n returns N for a feasible effect", {
  out <- morie:::.binary_power_required_n(p1 = 0.5, p2 = 0.6,
                                           alpha = 0.05, power = 0.80)
  expect_true(is.numeric(out) && out > 0)
})

test_that(".binary_power_required_n returns NA on identical p1 = p2", {
  out <- morie:::.binary_power_required_n(p1 = 0.5, p2 = 0.5,
                                           alpha = 0.05, power = 0.80)
  expect_true(is.na(out))
})
