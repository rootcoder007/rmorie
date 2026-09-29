# Coverage tests for R/bndpcb_native.R (Muller and Norets 2016): the
# truncated normal interval, coverage by region on the .ghc stream, the
# bettable-shortfall search and the bet-proof interval.

test_that("truncated normal interval and its empty case", {
  z <- qnorm(0.975)
  r <- morie_truncated_normal_interval(0.5)
  expect_equal(c(r$lower, r$upper), c(0, 0.5 + z))
  expect_false(r$empty)
  e <- morie_truncated_normal_interval(-3)
  expect_true(e$empty)
  expect_equal(c(e$lower, e$upper, e$width), c(0, 0, 0))
  f <- morie_truncated_normal_interval(4, level = 0.9, lower_bound = -1)
  expect_equal(c(f$lower, f$upper), 4 + c(-1, 1) * qnorm(0.95))
})

test_that("coverage by region replays the normal draws", {
  r <- morie_coverage_by_region(0.3, draws = 400, seed = 2, split = 0.5)
  x <- 0.3 + .ghc_norm(.ghc_rng(2), 400)
  z <- qnorm(0.975)
  lo <- pmax(x - z, 0)
  hi <- x + z
  ok <- !(hi < 0) & lo <= 0.3 & 0.3 <= hi
  expect_equal(r$marginal_coverage, mean(ok))
  expect_equal(r$subset_share, mean(x < 0.5))
  expect_equal(r$subset_coverage, mean(ok[x < 0.5]))
  expect_equal(r$mean_width, mean(pmax(hi - lo, 0)), tolerance = 1e-12)
  expect_equal(r$p_empty, mean(pmax(hi - lo, 0) <= 1e-12))
  expect_true(is.nan(morie_coverage_by_region(0.3, draws = 50, split = -100)$subset_coverage))
})

test_that("bettable shortfall search and the bet-proof interval", {
  g <- c(-1, 0, 0.5, 1)
  v <- morie_bet_violation(0.2, draws = 400, seed = 1, grid = g)
  sh <- vapply(g, function(c) {
    r <- morie_coverage_by_region(0.2, draws = 400, seed = 1, split = c)
    if (r$subset_share < 0.01 || is.nan(r$subset_coverage)) NA else 0.95 - r$subset_coverage
  }, 0)
  expect_equal(v$max_shortfall, max(c(0, sh), na.rm = TRUE))
  if (v$max_shortfall > 0) expect_equal(v$at_cut, g[which.max(sh)])
  expect_equal(v$bet_proof, v$max_shortfall <= 0.02)
  expect_length(morie_bet_violation(0.2, draws = 100)$level, 1L)
  z <- qnorm(0.975)
  b <- morie_bet_proof_interval(-3)
  expect_equal(c(b$lower, b$upper), c(0, z))
  expect_true(b$naive_empty)
  expect_true(b$widened)
  a <- morie_bet_proof_interval(3)
  expect_equal(c(a$lower, a$upper), 3 + c(-1, 1) * z)
  expect_false(a$widened)
  m <- morie_bet_proof_interval(0.1, min_width = 5)
  expect_equal(m$width, 5)
  expect_identical(morie_bndpcb, morie_bet_proof_interval)
  expect_identical(morie_pseudobayescredible, morie_bet_proof_interval)
  expect_identical(morie_bound_pseudo_credible, morie_bet_proof_interval)
  expect_error(morie_bet_proof_interval(1, min_width = 0), "min_width must be positive")
})
