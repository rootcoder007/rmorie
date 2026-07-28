# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the robust shelf. The deterministic
# estimators (Qn, Sn, Huber, Theil-Sen, the M-scale) are anchored to
# full-precision Python values on a shared LCG fixture; the
# subset-search estimators (S, MM, tau) draw from the language's own
# RNG and are tested against the structural claims -- breakdown under
# leverage contamination, efficiency ordering, the frozen scale.

rob_fixture <- function(n = 300L, s = 555) {
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[i] <- (s + 0.5) / 4294967296
  }
  stats::qnorm(u)
}

test_that("the fixture matches the one Python anchored against", {
  expect_equal(rob_fixture(3L),
               c(-0.12273223115773894, 0.30326639813224104,
                 0.9645900546976356), tolerance = 1e-12)
})

test_that("the calibration constants solve their defining equations", {
  # Qn's d = 1/(sqrt(2) qnorm(5/8)), recomputed
  expect_equal(.rob_qn_d, 1 / (sqrt(2) * stats::qnorm(5 / 8)),
               tolerance = 1e-12)
  # biweight at 1.5476: E_Phi[rho] = 1/2, the 50%-breakdown calibration
  v <- stats::integrate(function(u) .rob_tukey_rho(u, .rob_tukey_c_bdp) *
                          stats::dnorm(u), -10, 10)$value
  expect_equal(v, 0.5, tolerance = 2e-4)
  # Huber 1.345: 95% efficiency
  cc <- .rob_huber_c
  num <- stats::integrate(stats::dnorm, -cc, cc)$value
  den <- stats::integrate(function(u) pmin(pmax(u, -cc), cc)^2 *
                            stats::dnorm(u), -10, 10)$value
  expect_equal(num^2 / den, 0.95, tolerance = 2e-4)
})

test_that("morie_rob_qn and morie_rob_sn match morie.fn", {
  z <- rob_fixture()
  expect_equal(morie_rob_qn(z[1:100])$value, 0.9341813235597836,
               tolerance = 1e-10)
  expect_equal(morie_rob_qn(z[1:8])$value, 0.9646075339587125,
               tolerance = 1e-10)
  expect_equal(morie_rob_sn(z[1:100])$value, 0.8990048429766321,
               tolerance = 1e-10)
  expect_equal(morie_rob_sn(z[1:8])$value, 0.9333848004253749,
               tolerance = 1e-10)
  expect_equal(.rob_s_scale(1.5 * z[1:200]), 1.520599080345652,
               tolerance = 1e-9)
  expect_equal(.rob_mad(z[1:150]), 1.0035543736024166, tolerance = 1e-12)
})

test_that("Qn and Sn are consistent and survive 40% contamination", {
  set.seed(3)
  for (f in list(morie_rob_qn, morie_rob_sn)) {
    vals <- replicate(150, f(stats::rnorm(200, sd = 2))$value)
    expect_equal(mean(vals), 2, tolerance = 0.03)
  }
  x <- rob_fixture(100L)
  bad <- c(x[1:60], rep(1000, 40))
  for (f in list(morie_rob_qn, morie_rob_sn)) {
    expect_lt(f(bad)$value, 4 * f(x)$value)
    expect_equal(f(bad)$breakdown, 0.5)
  }
  expect_gt(stats::sd(bad), 100 * stats::sd(x))
  expect_error(morie_rob_qn(1), "at least 2")
  expect_error(morie_rob_sn(1), "at least 2")
})

test_that("the small-sample corrections keep both unbiased at n = 8", {
  set.seed(9)
  qn <- replicate(3000, morie_rob_qn(stats::rnorm(8))$value)
  sn <- replicate(3000, morie_rob_sn(stats::rnorm(8))$value)
  expect_equal(mean(qn), 1, tolerance = 0.08)
  expect_equal(mean(sn), 1, tolerance = 0.08)
})

test_that("morie_rob_huber matches morie.fn.hubrr", {
  z <- rob_fixture()
  x <- z[1:120]
  y <- 2 + 3 * x + 0.5 * z[121:240]
  o <- morie_rob_huber(x, y)
  expect_equal(o$beta, c(2.001830103653132, 2.90968315638123),
               tolerance = 1e-9)
  expect_equal(o$scale, 0.4506763485541621, tolerance = 1e-9)
  expect_equal(o$se, c(0.04901448187454551, 0.049856127703827566),
               tolerance = 1e-8)
  expect_true(o$converged)
  expect_equal(o$breakdown, 0)
})

test_that("Huber survives vertical outliers and breaks under leverage", {
  z <- rob_fixture()
  x <- z[1:120]
  y <- 2 + 3 * x + 0.5 * z[121:240]
  y2 <- y
  y2[1:24] <- y2[1:24] + 30       # 20% vertical
  o2 <- morie_rob_huber(x, y2)
  expect_equal(o2$beta[2L], 3, tolerance = 0.2)
  expect_lt(mean(o2$weights[1:24]), 0.2)
  # the documented failure: bad leverage
  x3 <- x
  y3 <- y
  x3[1:40] <- 8 + 0.1 * z[241:280]
  y3[1:40] <- -20
  bad <- morie_rob_huber(x3, y3)
  expect_gt(abs(bad$beta[2L] - 3), 2)
  # and the fixes fix it
  mm <- morie_rob_mm(x3, y3, n_subsets = 150, seed = 3)
  tau <- morie_rob_tau(x3, y3, n_subsets = 150, seed = 3)
  expect_equal(mm$beta, c(2, 3), tolerance = 0.3)
  expect_equal(tau$beta, c(2, 3), tolerance = 0.3)
  expect_equal(mm$breakdown, 0.5)
  expect_equal(tau$breakdown, 0.5)
})

test_that("the S-estimator seeds MM and the scale is frozen", {
  z <- rob_fixture()
  x <- z[1:150]
  y <- 2 + 3 * x + 0.5 * z[151:300]
  x[1:50] <- 8 + 0.1 * rob_fixture(50L, 77)
  y[1:50] <- -20
  s <- morie_rob_s(x, y, n_subsets = 150, seed = 5)
  expect_equal(s$beta, c(2, 3), tolerance = 0.35)
  expect_equal(s$gaussian_efficiency, 0.287)
  mm <- morie_rob_mm(x, y, n_subsets = 150, seed = 5)
  expect_equal(mm$beta_initial, s$beta, tolerance = 0.35)
  expect_true(mm$scale_held_fixed)
  expect_equal(mm$gaussian_efficiency, 0.95)
})

test_that("MM is more efficient than S on clean data", {
  set.seed(19)
  s_err <- mm_err <- numeric(40)
  for (i in 1:40) {
    x <- stats::rnorm(80)
    y <- 2 + 3 * x + stats::rnorm(80, sd = 0.5)
    s_err[i] <- morie_rob_s(x, y, n_subsets = 80, seed = i)$beta[2L] - 3
    mm_err[i] <- morie_rob_mm(x, y, n_subsets = 80, seed = i)$beta[2L] - 3
  }
  expect_lt(stats::var(mm_err), 0.7 * stats::var(s_err))
})

test_that("the MM alias shares the implementation exactly", {
  z <- rob_fixture()
  x <- z[1:100]
  y <- 2 + 3 * x + 0.5 * z[101:200]
  a <- morie_rob_mm_alias(x, y, n_subsets = 60, seed = 1)
  b <- morie_rob_mm(x, y, n_subsets = 60, seed = 1)
  expect_equal(a$beta, b$beta, tolerance = 1e-14)
  expect_equal(a$scale, b$scale, tolerance = 1e-14)
  expect_equal(a$alias_of, "morie_rob_mm")
})

test_that("morie_rob_m distinguishes monotone from redescending", {
  z <- rob_fixture()
  x <- z[1:120]
  y <- 2 + 3 * x + 0.5 * z[121:240]
  h <- morie_rob_m(x, y, psi = "huber")
  b <- morie_rob_m(x, y, psi = "bisquare")
  expect_true(h$monotone && h$unique_solution)
  expect_null(h$start_dependent_warning)
  expect_false(b$monotone)
  expect_true(grepl("LOCAL", b$start_dependent_warning))
  expect_equal(h$beta, c(2, 3), tolerance = 0.15)
  expect_equal(b$beta, c(2, 3), tolerance = 0.15)
  expect_error(morie_rob_m(x, y, psi = "cauchy"), "huber")
})

test_that("morie_rob_theil_sen matches morie.fn.theils", {
  z <- rob_fixture()
  x <- z[1:120]
  y <- 2 + 3 * x + 0.5 * z[121:240]
  o <- morie_rob_theil_sen(x, y)
  expect_equal(o$slope, 2.948551357070639, tolerance = 1e-10)
  expect_equal(o$intercept, 2.0434081698270496, tolerance = 1e-10)
  expect_equal(o$ci, c(2.8500740512680958, 3.0528500245756764),
               tolerance = 1e-9)
  expect_equal(o$n_pairs, 7140L)
  expect_equal(o$breakdown, 1 - 1 / sqrt(2), tolerance = 1e-12)
})

test_that("Theil-Sen excludes tied x pairs and validates", {
  o <- morie_rob_theil_sen(c(1, 1, 2, 3, 4), c(1, 1.2, 2, 3, 4))
  expect_equal(o$n_tied_x, 1L)
  expect_equal(o$n_pairs, 9L)
  expect_error(morie_rob_theil_sen(rep(1, 5), 1:5), "tied")
  expect_error(morie_rob_theil_sen(1:2, 1:2), "at least 3")
})

test_that("Sen's slope is Theil-Sen on the time index", {
  z <- rob_fixture(100L)
  y <- 0.5 * (0:59) + z[1:60]
  o <- morie_rob_sens_slope(y)
  ref <- morie_rob_theil_sen(0:59, y)
  expect_equal(o$slope, ref$slope, tolerance = 1e-14)
  expect_equal(o$ci, ref$ci, tolerance = 1e-14)
  expect_equal(o$trend, "increasing")
  expect_equal(o$alias_of, "morie_rob_theil_sen")
  flat <- morie_rob_sens_slope(z[1:60])
  expect_equal(flat$trend, "no trend at this alpha")
})

test_that("the subset searches do not leak the global RNG stream", {
  z <- rob_fixture()
  x <- z[1:80]
  y <- 2 + 3 * x + 0.5 * z[81:160]
  set.seed(777)
  before <- stats::runif(3L)
  set.seed(777)
  invisible(morie_rob_s(x, y, n_subsets = 30, seed = 1))
  invisible(morie_rob_mm(x, y, n_subsets = 30, seed = 1))
  invisible(morie_rob_tau(x, y, n_subsets = 30, seed = 1))
  expect_equal(stats::runif(3L), before, tolerance = 1e-12)
})
