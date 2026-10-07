# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P1 (continued): Le Cam's bound must agree with research/lean/P1LeCam.lean.

test_that("sum_min = 1 - TV, TV in [0, 1], two_point and minimax for random laws and estimators", {
  set.seed(1)
  for (rep in 1:60) {
    n <- sample(2:12, 1)
    p <- rexp(n); p <- p / sum(p)
    q <- rexp(n); q <- q / sum(q)
    tp <- rnorm(1); tq <- rnorm(1)
    b <- morie_two_point_bound(p, q, tp, tq)
    expect_equal(b$tv, sum(abs(p - q)) / 2, tolerance = 1e-12)
    expect_equal(b$sum_min, 1 - b$tv, tolerance = 1e-12)          # sum_min
    expect_gte(b$tv, 0); expect_lte(b$tv, 1)                        # tv_nonneg, tv_le_one
    expect_equal(b$delta, abs(tp - tq))
    expect_equal(b$bound, abs(tp - tq) * (1 - b$tv) / 2, tolerance = 1e-12)
    expect_null(b$risk_p)
    for (draw in 1:5) {
      Tt <- rnorm(n, (tp + tq) / 2, 2)
      e <- morie_two_point_bound(p, q, tp, tq, estimator = Tt)
      expect_equal(e$risk_p, sum(p * abs(Tt - tp)), tolerance = 1e-12)
      expect_gte(e$risk_p + e$risk_q, b$delta * (1 - b$tv) - 1e-12)   # two_point
      expect_gte(e$minimax_risk, b$bound - 1e-12)                      # minimax
      expect_true(e$satisfied)
      expect_equal(e$minimax_risk, max(e$risk_p, e$risk_q))
    }
  }
  same <- morie_two_point_bound(c(0.5, 0.5), c(0.5, 0.5), 0, 1)
  expect_equal(same$tv, 0); expect_equal(same$bound, 0.5)
  far <- morie_two_point_bound(c(1, 0), c(0, 1), 0, 1)
  expect_equal(far$tv, 1); expect_equal(far$bound, 0)
})

test_that("input checks", {
  expect_error(morie_two_point_bound(c(0.5, 0.5), c(1), 0, 1), "equal length")
  expect_error(morie_two_point_bound(c(0.5, 0.6), c(0.5, 0.5), 0, 1), "probability")
  expect_error(morie_two_point_bound(c(0.5, 0.5), c(0.5, 0.5), c(0, 1), 1), "single")
  expect_error(morie_two_point_bound(c(0.5, 0.5), c(0.5, 0.5), 0, 1, estimator = 1), "one value per point")
})
