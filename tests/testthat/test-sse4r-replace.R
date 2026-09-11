# Anchors for stochastic shared embedding replacement.
#
# The draw is taken from the generator returned by .ghc_rng(seed). Passing
# anything else in that position raises, which is what this module did for
# every p > 0, so the tests below run the non-identity branch on purpose
# rather than only the p = 0 early return.

test_that("p = 0 is exactly the identity", {
  idx <- c(3L, 1L, 4L, 1L, 5L)
  r <- .sse4r_sse_replace(idx, 10L, p = 0, seed = 1)
  expect_identical(unlist(r$indices), idx)
  expect_length(r$replaced, 0L)
  expect_identical(r$rate, 0.0)
  expect_identical(r$p, 0.0)
  expect_match(r$note, "identity")
})

test_that("a positive p runs the replacement branch at all", {
  # the branch raised "$ operator is invalid for atomic vectors" because the
  # seeded generator was discarded and a bare number was handed to .ghc_unif
  expect_silent(r <- .sse4r_sse_replace(0:9, 10L, p = 0.5, seed = 1))
  expect_length(r$indices, 10L)
  expect_true(all(is.finite(unlist(r$indices))))
})

test_that("every emitted index stays inside the table", {
  set.seed(1)
  idx <- sample(0:99, 400, TRUE)
  for (p in c(0.1, 0.5, 0.9, 1.0)) {
    out <- unlist(.sse4r_sse_replace(idx, 100L, p = p, seed = 7)$indices)
    expect_true(all(out >= 0L & out < 100L))
    expect_length(out, length(idx))
    expect_type(out, "integer")
  }
})

test_that("the replacement rate tracks p, discounted by self-replacement", {
  # a passing position is redrawn uniformly from the same table, so it lands
  # on its own value with probability 1/n: the observed rate of *change* is
  # p * (1 - 1/n), not p
  set.seed(2)
  idx <- sample(0:99, 4000, TRUE)
  for (p in c(0.1, 0.5, 0.9)) {
    r <- .sse4r_sse_replace(idx, 100L, p = p, seed = 11)
    expect_equal(r$rate, p * (1 - 1 / 100), tolerance = 0.02)
    expect_equal(r$rate, mean(unlist(r$indices) != idx), tolerance = 1e-12)
  }
})

test_that("the draw is seeded, so runs are reproducible and seeds differ", {
  idx <- rep(0:9, 20)
  a <- .sse4r_sse_replace(idx, 10L, p = 0.5, seed = 7)
  b <- .sse4r_sse_replace(idx, 10L, p = 0.5, seed = 7)
  d <- .sse4r_sse_replace(idx, 10L, p = 0.5, seed = 8)
  expect_identical(a$indices, b$indices)
  expect_false(identical(a$indices, d$indices))
  # discarding the generator would have made the seed irrelevant
  expect_false(identical(a$replaced, d$replaced))
})

test_that("the replacement log records only positions that actually moved", {
  set.seed(3)
  idx <- sample(0:49, 200, TRUE)
  r <- .sse4r_sse_replace(idx, 50L, p = 0.6, seed = 5)
  out <- unlist(r$indices)
  moved <- which(out != idx)
  expect_length(r$replaced, length(moved))
  for (rec in r$replaced) {
    pos0 <- rec[[1]]          # reported 0-based
    expect_identical(idx[pos0 + 1L], rec[[2]])
    expect_identical(out[pos0 + 1L], rec[[3]])
    expect_false(rec[[2]] == rec[[3]])
  }
})

test_that("the arguments are validated", {
  expect_error(.sse4r_sse_replace(0:4, 0L), "table is empty")
  expect_error(.sse4r_sse_replace(0:4, 10L, p = -0.1), "must lie in")
  expect_error(.sse4r_sse_replace(0:4, 10L, p = 1.5), "must lie in")
  expect_error(.sse4r_sse_replace(c(0L, 10L), 10L, p = 0.5), "outside the table")
  expect_error(.sse4r_sse_replace(c(-1L, 2L), 10L, p = 0.5), "outside the table")
})

test_that("p = 1 replaces every position", {
  idx <- rep(0:9, 10)
  r <- .sse4r_sse_replace(idx, 10L, p = 1, seed = 4)
  # every position was redrawn, so the rate is 1 - 1/n in expectation and
  # every entry not equal to its original is logged
  expect_gt(r$rate, 0.8)
  expect_equal(r$rate, mean(unlist(r$indices) != idx), tolerance = 1e-12)
})
