# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P12: Duncan-Davis bounds must agree with research/lean/P12Bounds.lean.

test_that("the Frechet interval contains every admissible joint table and both ends are attained", {
  set.seed(5)
  for (k in 1:200) {
    p <- runif(1, 0.05, 0.95); q <- runif(1)
    b <- morie_ecological_bounds(p, q)$neighbourhoods
    lo <- max(0, p + q - 1); hi <- min(p, q)                 # admissible joint masses (pq_ge, pq_le_p, pq_le_q)
    for (pq in c(lo, hi, runif(3, lo, hi))) {
      r <- pq / p
      expect_gte(r, b$lower - 1e-12); expect_lte(r, b$upper + 1e-12)   # dd_bounds
      rc <- (q - pq) / (1 - p)                                           # dd_complement
      expect_gte(rc, b$complement_lower - 1e-12); expect_lte(rc, b$complement_upper + 1e-12)
      expect_equal(q, p * r + (1 - p) * rc, tolerance = 1e-12)
    }
    expect_equal(lo / p, b$lower, tolerance = 1e-12); expect_equal(hi / p, b$upper, tolerance = 1e-12)  # ends_attained
    expect_equal(b$width, b$upper - b$lower)
  }
})

test_that("aggregate bounds are the m_g p_g-weighted means of the neighbourhood bounds", {
  p <- c(0.2, 0.5, 0.8); q <- c(0.1, 0.3, 0.6); w <- c(1000, 2000, 500)
  r <- morie_ecological_bounds(p, q, w)
  m <- w * p
  expect_equal(unname(r$aggregate["lower"]), sum(m * r$neighbourhoods$lower) / sum(m), tolerance = 1e-12)
  expect_equal(unname(r$aggregate["upper"]), sum(m * r$neighbourhoods$upper) / sum(m), tolerance = 1e-12)
  # any admissible per-neighbourhood rates give an aggregate inside (dd_aggregate_bounds)
  set.seed(6)
  for (k in 1:50) {
    rr <- runif(3, r$neighbourhoods$lower, r$neighbourhoods$upper)
    agg <- sum(m * rr) / sum(m)
    expect_gte(agg, r$aggregate["lower"] - 1e-12); expect_lte(agg, r$aggregate["upper"] + 1e-12)
  }
  expect_true(all(r$neighbourhoods$width >= 0))
  expect_error(morie_ecological_bounds(c(0.2, 0.5), 0.1), "equal length")
  expect_error(morie_ecological_bounds(1, 0.5), "p must")
  expect_error(morie_ecological_bounds(0.5, 0.5, weights = 0), "positive")
})
