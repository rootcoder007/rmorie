# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P11: the Imbens-Manski interval must agree with research/lean/P11Coverage.lean.

test_that("the cutoff solves the coverage equation and lies between the one- and two-sided quantiles", {
  set.seed(1)
  for (k in 1:60) {
    lo <- runif(1, -1, 1); up <- lo + runif(1, 0, 2)
    sl <- runif(1, 0.01, 0.5); su <- runif(1, 0.01, 0.5)
    level <- sample(c(0.8, 0.9, 0.95, 0.99), 1)
    r <- morie_bounds_confidence(lo, up, sl, su, level)
    alpha <- 1 - level
    expect_equal(pnorm(r$cutoff + r$delta) - pnorm(-r$cutoff), level, tolerance = 1e-9)
    expect_lte(r$cutoff, r$z_two_sided + 1e-9)                       # im_cutoff_between (upper)
    expect_lte(pnorm(-r$cutoff), alpha + 1e-9)                       # im_cutoff_between (lower)
    expect_gte(r$cutoff, r$z_one_sided - 1e-9)
    expect_equal(r$interval[["lower"]], lo - r$cutoff * sl)
    expect_equal(r$interval[["upper"]], up + r$cutoff * su)
    # the region interval (two-sided z) contains the Imbens-Manski one (region_coverage_le in spirit)
    expect_lte(r$region_interval[["lower"]], r$interval[["lower"]] + 1e-12)
    expect_gte(r$region_interval[["upper"]], r$interval[["upper"]] - 1e-12)
    if (r$delta > 0) expect_gt(r$coverage_two_sided, level)          # two_sided_overcovers
  }
})

test_that("a wider region needs a smaller cutoff; zero width gives the two-sided quantile (im_cutoff_antitone)", {
  deltas <- c(0, 0.1, 0.5, 1, 2, 4, 8)
  cuts <- vapply(deltas, function(d) morie_bounds_confidence(0, d * 0.1, 0.1, 0.1, 0.95)$cutoff, numeric(1))
  expect_true(all(diff(cuts) <= 1e-9))
  expect_equal(cuts[1], qnorm(0.975), tolerance = 1e-8)
  expect_equal(cuts[length(cuts)], qnorm(0.95), tolerance = 1e-6)   # wide region: one-sided limit
  expect_error(morie_bounds_confidence(1, 0, 0.1, 0.1), "at least")
  expect_error(morie_bounds_confidence(0, 1, 0, 0.1), "positive")
})

test_that("the cutoff exists inside the search bracket and is the only root (im_cutoff_exists, im_cutoff_unique)", {
  set.seed(7)
  for (k in 1:40) {
    delta <- runif(1, 0, 6); alpha <- runif(1, 0.01, 0.3)
    f <- function(c_) pnorm(c_ + delta) - pnorm(-c_) - (1 - alpha)
    z1 <- qnorm(1 - alpha); z2 <- qnorm(1 - alpha / 2)
    # a sign change on the bracket the function searches: existence
    expect_lte(f(z1 - 1e-9), 1e-12); expect_gte(f(z2 + 1e-9), -1e-12)
    r <- morie_bounds_confidence(0, delta * 0.1, 0.1, 0.1, 1 - alpha)
    expect_equal(f(r$cutoff), 0, tolerance = 1e-9)
    # strict monotonicity: every other c misses, on both sides
    for (d in c(-1, -0.1, 0.1, 1)) expect_gt(abs(f(r$cutoff + d)), 1e-6)
    expect_true(all(c("Research.P11.im_cutoff_exists", "Research.P11.im_cutoff_unique") %in% r$theorems))
  }
})
