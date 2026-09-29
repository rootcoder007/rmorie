# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/pmpfit_native.R (two-parameter Poisson-Dirichlet,
# Pitman & Yor 1997). Stick-breaking weights are recomputed from the
# same Beta draws on the package stream, the predictive rule from
# Definition 1, and the expected cluster count from its recursion.

test_that("check validates 0 <= alpha < 1 and theta > -alpha", {
  r <- morie_pmpfit_check(0.3, 1)
  expect_equal(c(r$alpha, r$theta), c(0.3, 1))
  expect_false(r$is_dirichlet)
  expect_true(morie_pmpfit_check(0, 2)$is_dirichlet)
  expect_true(morie_pmpfit_check(0.5, -0.4)$theta < 0)
  expect_error(morie_pmpfit_check(1, 1), "0 <= alpha < 1")
  expect_error(morie_pmpfit_check(-0.1, 1), "0 <= alpha < 1")
  expect_error(morie_pmpfit_check(0.5, -0.5), "theta > -alpha")
})

test_that("stick breaking uses Beta(1 - alpha, theta + k alpha), or the DP form at alpha = 0", {
  for (fn in list(morie_pmpfit, pmpfit, pmp_fit, pitman_yor)) {
    r <- fn(0.3, 1.5, 5, seed = 2)
    e <- .ghc_rng(2)
    y <- vapply(1:5, function(k) .ghc_beta(e, 0.7, 1.5 + k * 0.3), 0)
    expect_equal(r$Y, y, tolerance = 1e-12)
    expect_equal(r$weights, y * cumprod(c(1, 1 - y[-5])), tolerance = 1e-12)
    expect_equal(r$remaining, prod(1 - y), tolerance = 1e-12)
    expect_equal(r$kept_mass + r$remaining, 1, tolerance = 1e-12)
  }
  d <- morie_pmpfit(0, 2, 4, seed = 7)
  e <- .ghc_rng(7)
  yd <- vapply(1:4, function(k) 1 - .ghc_unif(e, 1L)^(1 / 2), 0)
  expect_equal(d$Y, yd, tolerance = 1e-12)
  expect_true(all(d$weights > 0))
  expect_error(morie_pmpfit(0.3, 1, 0), "at least one stick")
})

test_that("predictive discounts each cluster by alpha and funds the new one", {
  cnt <- c(5, 3, 1)
  p <- morie_pmpfit_predictive(cnt, 0.4, 2)
  expect_equal(p$occupied, (cnt - 0.4) / 11, tolerance = 1e-12)
  expect_equal(p$new, (2 + 3 * 0.4) / 11, tolerance = 1e-12)
  expect_equal(p$total, 1, tolerance = 1e-12)
  expect_equal(p$discount_transferred, 3 * 0.4 / 11, tolerance = 1e-12)
  # alpha = 0 is the Chinese restaurant process
  d <- morie_pmpfit_predictive(cnt, 0, 2)
  expect_equal(d$occupied, cnt / 11)
  expect_equal(d$new, 2 / 11)
  expect_error(morie_pmpfit_predictive(c(2, 0), 0.1, 1), "positive count")
  # a cluster no larger than the discount would take negative mass
  expect_error(morie_pmpfit_predictive(c(5, 0.25), 0.4, 1), "negative weight")
})

test_that("expected cluster count follows E[K_n] = sum (theta + alpha E[K_i]) / (theta + i)", {
  ek <- 0
  for (i in 0:9) ek <- ek + (1.5 + ek * 0.4) / (1.5 + i)
  r <- morie_pmpfit_expected(10, 0.4, 1.5)
  expect_equal(r$expected, ek, tolerance = 1e-12)
  expect_identical(r$regime, "power law n^alpha")
  d <- morie_pmpfit_expected(10, 0, 1.5)
  ed <- sum(1.5 / (1.5 + 0:9))
  expect_equal(d$expected, ed, tolerance = 1e-12)
  expect_identical(d$regime, "logarithmic theta log n")
  expect_equal(morie_pmpfit_expected(1, 0.4, 1.5)$expected, 1)
  expect_error(morie_pmpfit_expected(0, 0, 1), "n must be at least 1")
  t <- morie_pmpfit_tail(50, theta = 1, alphas = c(0, 0.3, 0.6))
  expect_equal(unlist(t$expected_clusters),
               setNames(vapply(c(0, 0.3, 0.6), function(a) morie_pmpfit_expected(50, a, 1)$expected, 0),
                        c("0", "0.3", "0.6")), tolerance = 1e-12)
  expect_true(t$monotone_in_alpha)
  # more discount, more types: the monotonicity the note claims
  expect_lt(t$expected_clusters[["0"]], t$expected_clusters[["0.6"]])
})

test_that("morie_pmpfit_cheatsheet states the drifting Beta", {
  expect_match(morie_pmpfit_cheatsheet(), "Beta(1 - alpha, theta + n alpha)", fixed = TRUE)
})
