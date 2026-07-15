# SPDX-License-Identifier: AGPL-3.0-or-later
# Property-based invariants for the native matcher, across random DGPs.
library(testthat)
library(rmorie)

test_that("matching invariants hold across random DGPs", {
  for (seed in c(1L, 17L, 99L, 1234L)) {
    set.seed(seed)
    n <- sample(200:800, 1)
    x1 <- rnorm(n); x2 <- runif(n)
    d <- rbinom(n, 1, plogis(x1 - x2))
    df <- data.frame(d = d, x1 = x1, x2 = x2)
    m <- sample(1:2, 1)
    r <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"),
                                         n_neighbors = m)
    p <- r$match_pairs
    # (a) pair count bounded by n_treated * m and by n_control
    expect_lte(nrow(p), sum(d == 1) * m)
    expect_lte(nrow(p), sum(d == 0))
    # (b) every pair's members exist and have the right arms
    expect_true(all(df[p$treated_idx, "d"] == 1))
    expect_true(all(df[p$control_idx, "d"] == 0))
    # (c) no control reused without replacement
    expect_false(any(duplicated(p$control_idx)))
    # (d) distances are finite and non-negative
    expect_true(all(is.finite(p$distance) & p$distance >= 0))
    # (e) matched_data contains exactly the matched units
    expect_setequal(rownames(r$matched_data),
                    unique(c(p$treated_idx, p$control_idx)))
  }
})
