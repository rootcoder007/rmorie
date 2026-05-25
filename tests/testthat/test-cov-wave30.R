# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 30 -- direct unit tests for the optimiser objectives
# extracted in the second refactoring batch (garch / tgarch / ghebp /
# ghlgd / hrzb1 / vrgft). Each helper is exercised on both its guard
# branches and a valid-parameter path.

test_that("GARCH / GJR-GARCH objectives: parameter-domain guard + valid", {
  r <- rnorm(40)
  expect_equal(morie:::.garch_negll(c(-1, 0.1, 0.8), r, 40L), 1e10) # omega<=0
  expect_equal(morie:::.garch_negll(c(0.1, 0.6, 0.6), r, 40L), 1e10) # a+b>=1
  expect_true(is.finite(morie:::.garch_negll(c(0.1, 0.1, 0.8), r, 40L)))
  expect_equal(morie:::.tgarch_negll(c(-1, 0.1, 0, 0.8), r, 40L), 1e10)
  expect_true(is.finite(morie:::.tgarch_negll(
    c(0.1, 0.05, 0.05, 0.8),
    r, 40L
  )))
})

test_that("Dirichlet-process / log-spline objectives are finite + valid", {
  expect_true(is.finite(morie:::.ghebp_negll(1.5, 5L, 40L)))
  expect_true(is.finite(morie:::.ghebp_negll(0.01, 8L, 100L)))
  gz <- seq(-3, 3, length.out = 41)
  basis <- function(u) vapply(seq_len(3), function(k) u^k, numeric(length(u)))
  Bx <- basis(rnorm(30))
  Bg <- basis(gz)
  expect_true(is.finite(morie:::.ghlgd_negll(rep(0, 3), Bx, Bg, gz, 30L)))
})

test_that("Manski maximum-score objective returns a bounded score", {
  X <- matrix(rnorm(40), 20, 2)
  ys <- rep(c(-1, 1), 10)
  s <- morie:::.hrzb1_score(c(1, -1), ys, X)
  expect_true(is.finite(s) && s >= -1 && s <= 1)
})

test_that("variogram model + WLS objective cover every model branch", {
  for (m in c("exponential", "gaussian", "spherical")) {
    expect_true(is.numeric(morie:::.vrgft_model(1:3, 0, 1, 2, m)))
  }
  expect_error(
    morie:::.vrgft_model(1, 0, 1, 2, "no-such-model"),
    "unknown model"
  )
  expect_true(is.finite(
    morie:::.vrgft_obj(
      c(0, 1, 2), 1:3, c(0.4, 0.6, 0.8),
      c(1, 1, 1), "exponential"
    )
  ))
})
