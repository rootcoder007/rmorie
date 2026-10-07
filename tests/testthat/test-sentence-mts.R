# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P11: MTS bounds must agree with research/lean/P11Selection.lean.

test_that("MTS: the naive difference is an upper bound; every MTS-consistent completion lies inside", {
  set.seed(7)
  for (k in 1:40) {
    n <- 400
    z <- sample(c("a", "b"), n, replace = TRUE)
    y <- rbinom(n, 1, ifelse(z == "b", 0.6, 0.3))
    r <- morie_sentence_effect_mts(y, z)
    m_b <- mean(y[z == "b"]); m_a <- mean(y[z == "a"]); pzb <- mean(z == "b"); pza <- 1 - pzb
    expect_equal(r$naive_difference, m_b - m_a, tolerance = 1e-12)
    expect_equal(unname(r$ate_bounds_mts["upper"]), m_b - m_a, tolerance = 1e-12)     # mts_ate_le_naive
    expect_equal(unname(r$mean_b_bounds), c(pzb * m_b, m_b), tolerance = 1e-12)       # mts_mean_b_le + worst case
    expect_equal(unname(r$mean_a_bounds), c(m_a, pza * m_a + pzb), tolerance = 1e-12) # mts_mean_a_ge + worst case
    # fill the unobserved arms so that MTS holds (the b group at least as high under both sentences)
    for (rep in 1:5) {
      yb <- y; ya <- y
      # y(b) for the a group: no higher on average than the b group's observed mean
      ya_b <- rbinom(sum(z == "a"), 1, runif(1, 0, m_b)); yb[z == "a"] <- ya_b
      # y(a) for the b group: at least the a group's observed mean
      yb_a <- rbinom(sum(z == "b"), 1, runif(1, m_a, 1)); ya[z == "b"] <- yb_a
      mts <- mean(ya[z == "b"]) >= mean(ya[z == "a"]) && mean(yb[z == "b"]) >= mean(yb[z == "a"])
      if (!mts) next
      ate <- mean(yb) - mean(ya)
      expect_lte(ate, r$ate_bounds_mts["upper"] + 1e-12)
      expect_gte(ate, r$ate_bounds_mts["lower"] - 1e-12)
      expect_lte(mean(yb), r$mean_b_bounds["upper"] + 1e-12); expect_gte(mean(ya), r$mean_a_bounds["lower"] - 1e-12)
    }
    expect_equal(unname(r$ate_bounds_mtr_mts), c(0, max(0, m_b - m_a)))               # mtr_mts_bounds
  }
})
