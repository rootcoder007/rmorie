# Truncated stick-breaking (Sethuraman 1994): Beta(1, alpha) sticks by
# inversion replayed on the shared stream, the (alpha/(1+alpha))^K
# expected tail and its inversion, decay diagnostics and the truncated
# DP draw.

test_that("sticks are V_k prod(1 - V_l) with V = 1 - U^(1/alpha)", {
  sb <- stick_breaking(2, 6, seed = 4)
  e <- .ghc_rng(4)
  V <- vapply(1:6, function(i) 1 - min(max(.ghc_unif(e, 1L), 1e-15), 1 - 1e-15)^(1 / 2), 1)
  p <- V * cumprod(c(1, 1 - V[-6]))
  expect_equal(sb$V, V, tolerance = 1e-15)
  expect_equal(sb$weights, p, tolerance = 1e-15)
  expect_equal(sb$remaining, prod(1 - V), tolerance = 1e-15)
  expect_equal(sb$kept_mass + sb$remaining, 1, tolerance = 1e-15)
  expect_error(stick_breaking(0, 3), "alpha must be positive")
  expect_error(stick_breaking(1, 0), "at least one stick")
  # E[remaining] = (alpha/(1+alpha))^K across many seeds
  rem <- vapply(1:2000, function(s) stick_breaking(2, 4, seed = s)$remaining, 1)
  expect_lt(abs(mean(rem) - (2 / 3)^4), 4 * stats::sd(rem) / sqrt(2000))
})

test_that("the truncation tail and the sticks needed for a tolerance", {
  te <- truncation_error(3, 10)
  expect_equal(te$expected_tail, 0.75^10, tolerance = 1e-15)
  expect_error(truncation_error(1, 0), "K >= 1")
  st <- sticks_for_tolerance(3, 1e-3)
  expect_identical(st$K, as.integer(ceiling(log(1e-3) / log(0.75))))
  expect_lte(st$expected_tail, 1e-3)
  expect_gt(truncation_error(3, st$K - 1)$expected_tail, 1e-3)
  expect_error(sticks_for_tolerance(3, 1), "tolerance must lie")
  expect_error(sticks_for_tolerance(0), "positive")
})

test_that("decay diagnostics compare the realised and expected tails", {
  d <- decay_diagnostics(c(0.5, 0.1, 0.3), alpha = 1)
  expect_equal(d$realised_tail, 0.1, tolerance = 1e-15)
  expect_equal(d$expected_tail, 0.125)
  expect_equal(d$ratio, 0.8, tolerance = 1e-14)
  expect_false(d$monotone)
  expect_identical(d$largest_index, 1L)
  expect_true(decay_diagnostics(c(0.5, 0.3), 1)$monotone)
  expect_error(decay_diagnostics(numeric(0), 1), "no weights")
})

test_that("the truncated DP renormalises the kept sticks and draws atoms", {
  r <- truncated_dp(1.5, 5, base_sampler = function(e) .ghc_norm(e, 1L), seed = 7)
  e <- .ghc_rng(7)
  sb <- stick_breaking(1.5, 5, rng = e)
  atoms <- vapply(1:5, function(i) .ghc_norm(e, 1L), 1)
  expect_equal(r$weights, sb$weights / sum(sb$weights), tolerance = 1e-15)
  expect_equal(r$atoms, atoms, tolerance = 1e-15)
  expect_equal(r$discarded_mass, sb$remaining, tolerance = 1e-15)
  raw <- truncated_dp(1.5, 5, seed = 7, renormalise = FALSE)
  expect_equal(raw$weights, stick_breaking(1.5, 5, seed = 7)$weights, tolerance = 1e-15)
  expect_equal(raw$atoms, 0:4)
  expect_equal(dp_truncation(1.5, 5, seed = 7)$weights, truncated_dp(1.5, 5, seed = 7)$weights)
  expect_identical(morie_slowdp$truncation_error, truncation_error)
})
