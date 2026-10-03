# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P17 (continued): desistance and replacement must agree with research/lean/P17Replacement.lean.

test_that("prevented_le_const, prevented_ge_const, later_sentence_prevents_less, prevented_net_le on random paths", {
  set.seed(17)
  for (rep in 1:60) {
    len <- sample(4:20, 1)
    lam <- sort(runif(len, 0, 20), decreasing = TRUE)
    S <- sample(1:(len - 2), 1)
    t0 <- sample(0:(len - S - 2), 1)
    r <- runif(1)
    out <- morie_incapacitation_career(lam, t0, S, replacement = r)
    expect_equal(out$prevented, sum(lam[t0 + 1:S]), tolerance = 1e-12)
    expect_equal(out$upper, S * lam[t0 + 1], tolerance = 1e-12)
    expect_equal(out$lower, S * lam[t0 + S + 1], tolerance = 1e-12)
    expect_lte(out$prevented, out$upper + 1e-12)                 # prevented_le_const
    expect_gte(out$prevented, out$lower - 1e-12)                 # prevented_ge_const
    expect_lte(out$later, out$prevented + 1e-12)                 # later_sentence_prevents_less
    expect_equal(out$later, sum(lam[t0 + 1 + 1:S]), tolerance = 1e-12)
    expect_equal(out$prevented_net, (1 - r) * out$prevented, tolerance = 1e-12)
    expect_lte(out$prevented_net, out$upper + 1e-12)             # prevented_net_le
    expect_equal(out$factor, 1 - r)
    # replaced_antitone: more replacement, fewer crimes prevented
    out2 <- morie_incapacitation_career(lam, t0, S, replacement = min(1, r + 0.1))
    expect_lte(out2$prevented_net, out$prevented_net + 1e-12)
  }
})

test_that("constant_rate and replaced_full; later is NA at the end of the path", {
  out <- morie_incapacitation_career(rep(3, 6), t0 = 1, S = 4)
  expect_equal(out$prevented, 12)                                 # constant_rate
  expect_equal(out$upper, 12); expect_equal(out$lower, 12)
  expect_true(is.na(out$later))
  full <- morie_incapacitation_career(rep(3, 6), t0 = 1, S = 4, replacement = 1)
  expect_equal(full$prevented_net, 0)                             # replaced_full
})

test_that("input checks", {
  lam <- c(5, 4, 3, 2, 1)
  expect_error(morie_incapacitation_career(c(1, -1), 0, 1), "non-negative")
  expect_error(morie_incapacitation_career(c(1, 2, 3), 0, 1), "non-increasing")
  expect_error(morie_incapacitation_career(lam, -1, 1), "t0")
  expect_error(morie_incapacitation_career(lam, 0.5, 1), "t0")
  expect_error(morie_incapacitation_career(lam, 0, 0), "S must")
  expect_error(morie_incapacitation_career(lam, 0, 1, replacement = 2), "replacement")
  expect_error(morie_incapacitation_career(lam, 3, 2), "length at least")
})
