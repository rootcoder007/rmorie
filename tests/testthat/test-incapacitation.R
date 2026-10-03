# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P17: incapacitation arithmetic must agree with research/lean/P17Incapacitation.lean.

test_that("the steady-state rate, the prevented share, monotonicity and the marginal year follow the theorems", {
  set.seed(18)
  for (k in 1:60) {
    lam <- runif(1, 0.1, 20); q <- runif(1, 0.01, 1); S <- runif(1, 0, 10)
    r <- morie_incapacitation(lam, q, S)
    fbar <- 1 / (lam * q)
    expect_equal(r$rate, lam * fbar / (fbar + S), tolerance = 1e-12)                       # steady_state_rate
    expect_equal(r$prevented_share, 1 - r$rate / lam, tolerance = 1e-12)                   # prevented_share_eq
    expect_lt(r$prevented_share, 1); expect_gte(r$prevented_share, 0)                       # prevented_share_lt_one
    expect_lte(r$rate, lam)                                                                 # rate_le_lam
    expect_lte(morie_incapacitation(lam, q, S + 1)$rate, r$rate)                            # rate_antitone_in_S
    expect_lte(morie_incapacitation(lam, min(1, q + 0.1), S)$rate, r$rate)                  # rate_antitone_in_q
    expect_equal(r$marginal_prevention, r$rate - morie_incapacitation(lam, q, S + 1)$rate, tolerance = 1e-12)  # marginal_prevention_eq
    expect_gt(r$marginal_prevention, 0)
    expect_gte(r$marginal_prevention, morie_incapacitation(lam, q, S + 1)$marginal_prevention)   # diminishing
    # cycle_rate: N cycles with free times f_i and crimes lam f_i give lam fbar / (fbar + S)
    f <- rexp(50, lam * q)
    expect_equal(sum(lam * f) / sum(f + S), lam * mean(f) / (mean(f) + S), tolerance = 1e-12)
  }
  g <- morie_incapacitation(lambda = c(2, 10), q = 0.1, S = 1, shares = c(0.8, 0.2))
  expect_lte(g$prevented_share[1], g$prevented_share[2])                                   # high_rate_more_prevented
  agg <- attr(g, "aggregate")
  expect_equal(unname(agg["free_rate"]), 0.8 * 2 + 0.2 * 10)
  expect_equal(unname(agg["incapacitated_rate"]), 0.8 * 2 / 1.2 + 0.2 * 10 / 2, tolerance = 1e-12)
  expect_error(morie_incapacitation(0, 0.1, 1), "positive")
  expect_error(morie_incapacitation(c(1, 2), 0.1, 1, shares = c(0.5, 0.6)), "summing to one")
})
