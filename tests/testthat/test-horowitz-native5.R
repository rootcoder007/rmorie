# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for Lewbel's special-regressor estimator.
# Anchors printed from morie.fn.hrzlew at full double precision --
# testthat tolerances are RELATIVE, so a rounded anchor silently
# weakens the test.
#
# The fixture is a low-discrepancy (golden-ratio) sequence pushed
# through the normal quantile rather than an RNG draw: it looks like a
# normal sample to the estimator but is bit-identical in both
# languages, which is what makes an exact anchor possible at all.

lew_fixture <- function(n = 800L) {
  i <- seq_len(n)
  q <- function(a) stats::qnorm((i * a) %% 1)
  x1 <- q(0.6180339887498949)
  v <- 6 * q(0.4142135623730951)
  x <- cbind(1, x1)
  y <- as.numeric(v + x %*% c(0, 1) + 0.8 * q(0.3178372451964559) > 0)
  list(x = x, y = y, v = v, x1 = x1, q = q)
}

test_that("morie_lewbel_binary matches morie.fn.hrzlew", {
  f <- lew_fixture()
  o <- morie_lewbel_binary(f$x, f$y, f$v)
  expect_equal(o$beta, c(0.07981580888259922, 0.9735122246699587),
               tolerance = 1e-9)
  expect_equal(o$se, c(0.14112712645775163, 0.14181821945031953),
               tolerance = 1e-9)
  expect_equal(o$bandwidth, 1.6631496108384336, tolerance = 1e-12)
  expect_equal(o$min_density, 0.0005780450703650931, tolerance = 1e-10)
  expect_equal(o$max_weight, 1729.9689094630637, tolerance = 1e-9)
  expect_equal(o$coefficient_on_V, 1)
  expect_true(o$root_n_consistent)
  expect_true(o$heteroskedasticity_allowed)
  expect_false(o$endogenous)
  # the design has beta = (0, 1): the estimator finds it
  expect_lt(abs(o$beta[2] - 1), 0.1)
})

test_that("the normal shortcut matches Python and lands near the kernel fit", {
  f <- lew_fixture()
  b <- morie_lewbel_binary(f$x, f$y, f$v, density = "normal")
  expect_equal(b$beta, c(0.07808014336176806, 0.9368934220540123),
               tolerance = 1e-9)
  expect_equal(b$min_density, 0.00046748207478284346, tolerance = 1e-10)
  expect_null(b$bandwidth)
  # U really is normal here, so Estimator 1 step 2's parametric
  # shortcut and the kernel density must agree
  expect_lt(abs(b$beta[2] - morie_lewbel_binary(f$x, f$y, f$v)$beta[2]), 0.1)
})

test_that("instruments switch morie_lewbel_binary to two-stage least squares", {
  f <- lew_fixture()
  iv <- 0.9 * f$x1 + 0.4 * f$q(0.2679491924311227)
  o <- morie_lewbel_binary(f$x, f$y, f$v, instruments = iv)
  expect_equal(o$beta, c(0.07969344393032551, 1.1103925359903162),
               tolerance = 1e-9)
  expect_true(o$endogenous)
  expect_equal(o$d, 2L)
})

test_that("the indicator direction is I(V >= 0), not I(V < 0)", {
  # Dong and Lewbel Corollary 1 gives T = [D - I(V >= 0)]/f(U). At
  # least one secondary description of this estimator states
  # I(V < 0); that is a different estimand, and it does not recover
  # beta on a design where the correct one does.
  f <- lew_fixture()
  o <- morie_lewbel_binary(f$x, f$y, f$v)
  vc <- f$v - mean(f$v)
  u <- as.numeric(vc - f$x %*% qr.coef(qr(f$x), vc))
  h <- .hrz_silverman(u)
  fh <- rowSums(.hrz_gauss_kernel(outer(u, u, "-") / h)) / (length(u) * h)
  flipped <- qr.coef(qr(f$x), (f$y - as.numeric(f$v < 0)) / fh)
  expect_lt(abs(o$beta[2] - 1), abs(flipped[2] - 1))
})

test_that("morie_lewbel_binary reports the weight it places on the tails", {
  f <- lew_fixture()
  o <- morie_lewbel_binary(f$x, f$y, f$v)
  # 1/f(U) is a genuine weight, large in the tails: the estimator's
  # known fragility, reported rather than hidden
  expect_equal(o$max_weight, 1 / o$min_density, tolerance = 1e-12)
  expect_gt(o$max_weight, 1)
})

test_that("morie_lewbel_binary validates its inputs", {
  f <- lew_fixture(200L)
  expect_error(morie_lewbel_binary(f$x, f$y, f$v[1:50]), "one entry per row")
  expect_error(morie_lewbel_binary(f$x, f$y * 2, f$v), "binary 0/1")
  expect_error(morie_lewbel_binary(f$x, f$y, f$v, density = "cauchy"),
               "nonparametric")
  expect_error(morie_lewbel_binary(f$x, f$y, f$v, bandwidth = -1), "positive")
  expect_error(morie_lewbel_binary(f$x[1:5, ], f$y[1:5], f$v[1:5]),
               "at least 10 observations")
})
