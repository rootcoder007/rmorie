# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Horowitz deconvolution / average
# derivative / NPIV mirrors. Anchors are printed from the Python
# modules at full double precision -- testthat tolerances are
# RELATIVE, so a rounded anchor silently weakens the test.

test_that("morie_deconvolution matches morie.fn.hrzdeconv", {
  w <- seq(-3, 3, length.out = 300)
  out <- morie_deconvolution(w, 0.4, grid = c(0, 1))
  expect_equal(out$density[1], 0.16632308283809408, tolerance = 1e-8)
  expect_equal(out$density[2], 0.1670657020886628, tolerance = 1e-8)
  expect_equal(out$regime, "supersmooth")
  expect_equal(morie_deconvolution(w, 0.4)$bandwidth,
               0.16748600134370448, tolerance = 1e-12)
  lap <- morie_deconvolution(w, 0.4, error = "laplace", grid = 0.5)
  expect_equal(lap$density[1], 0.16670013390208968, tolerance = 1e-8)
  expect_equal(lap$regime, "ordinary smooth")
  expect_error(morie_deconvolution(w, -1), "positive")
  expect_error(morie_deconvolution(w[1:4], 0.4), "at least 8")
})

test_that("the deconvolution default bandwidth beats a naive KDE", {
  # The point of the criterion-based default: a cut-off fixed at
  # n^{-1/8} over-smooths so badly that deconvolving is worse than
  # not deconvolving. Judged on integrated squared error over a grid,
  # not one point, because a contaminated density is over-dispersed
  # rather than uniformly low.
  set.seed(11)
  n <- 3000
  s <- 0.4
  w <- rnorm(n) + rnorm(n) * s
  g <- seq(-3, 3, length.out = 61)
  truth <- dnorm(g)
  dec <- morie_deconvolution(w, s, grid = g)$density
  hb <- 1.06 * stats::sd(w) * n^(-0.2)
  naive <- vapply(g, function(u) mean(dnorm((u - w) / hb)) / hb, numeric(1))
  ise <- function(d) sum(diff(g) * (utils::head((d - truth)^2, -1L) +
                                      utils::tail((d - truth)^2, -1L)) / 2)
  expect_lt(ise(dec), ise(naive))
})

test_that("morie_deconv_rate and morie_deconv_normality match Python", {
  dr <- morie_deconv_rate(1e6)
  expect_equal(dr$ratio, 5239213805.878165, tolerance = 1e-9)
  expect_equal(dr$logarithmic_rate, 0.005239213805878165, tolerance = 1e-12)
  expect_equal(dr$regime, "supersmooth")
  expect_equal(morie_deconv_rate(1000, error = "laplace")$regime,
               "ordinary smooth")
  nm <- morie_deconv_normality(0.41, 0.399, 1000, 0.2, 2)
  expect_equal(nm$z, 0.10999999999999954, tolerance = 1e-10)
  expect_equal(nm$scaling, 10)
  expect_equal(nm$p_two_sided, 0.9124093749153668, tolerance = 1e-10)
  expect_error(morie_deconv_normality(0.4, 0.4, 1000, -1, 2), "positive h, b")
})

test_that("morie_average_derivative matches morie.fn.hrzade", {
  x <- seq(-2, 2, length.out = 200)
  y <- 2 * x + cos(x)
  ad <- morie_average_derivative(x, y)
  expect_equal(ad$delta, 0.4151733444765248, tolerance = 1e-10)
  expect_equal(ad$se, 0.041815409055812804, tolerance = 1e-10)
  expect_equal(ad$bandwidth, 0.42739475143902167, tolerance = 1e-12)
  ah <- morie_average_derivative_hat(x, y)
  expect_equal(ah$delta_hat, 0.4349150071829417, tolerance = 1e-10)
  expect_equal(ah$se, 0.051582952486038086, tolerance = 1e-10)
  expect_equal(ah$bandwidth, 0.3279273842112686, tolerance = 1e-12)
  expect_true(ah$undersmoothed)
})

test_that("the average derivative recovers the density-weighted estimand", {
  # E(Y|X) = 2X with X standard normal: the DENSITY-WEIGHTED
  # estimand is 2 * integral of phi^2 = 2 / (2 sqrt(pi)) = 0.5642,
  # not the unweighted 2. A sign slip in K' returns -0.5642 and would
  # pass any test that only checked the magnitude.
  set.seed(3)
  x <- rnorm(4000)
  ad <- morie_average_derivative(x, 2 * x)
  expect_equal(ad$delta, 1 / sqrt(pi), tolerance = 0.08)
  expect_gt(ad$delta, 0)
})

test_that("morie_tikhonov_iv and morie_sieve_iv match Python", {
  tm <- rbind(c(1, 0.5, 0.2), c(0.5, 1, 0.3), c(0.2, 0.3, 1), c(0.1, 0, 0.4))
  b <- c(1, 2, 0.5, 0.3)
  tk <- morie_tikhonov_iv(tm, b)
  expect_equal(tk$g, c(0.07329999439420307, 1.8990137914345633,
                       0.03674930232627904), tolerance = 1e-10)
  expect_equal(tk$alpha, 0.001)
  expect_equal(tk$residual_norm, 0.3093339272614306, tolerance = 1e-10)
  expect_equal(tk$condition_number, 11.484615128723563, tolerance = 1e-8)
  expect_true(tk$ill_posed)
  # the L-curve trade-off must be monotone: more penalty, smaller
  # solution norm and larger residual
  expect_true(all(diff(tk$l_curve[, 3]) < 0))
  expect_true(all(diff(tk$l_curve[, 2]) > 0))
  expect_error(morie_tikhonov_iv(tm, b, alphas = c(0, 1)), "positive")

  sv <- morie_sieve_iv(tm, b)
  expect_equal(sv$g, c(0.07309184993531646, 1.9197930142302737, 0),
               tolerance = 1e-10)
  expect_equal(sv$K, 2L)
  expect_equal(sv$residual_norm, 0.31122800633547576, tolerance = 1e-10)
  expect_equal(sv$condition_number_at_K, 9.161219386939747, tolerance = 1e-8)
  expect_error(morie_sieve_iv(tm, b, K = 9), "must lie in 1..3")

  qv <- morie_npiv_quantile(tm, b, tau = 0.25)
  expect_equal(qv$g, sv$g, tolerance = 1e-12)
  expect_true(qv$nonlinear)
  expect_error(morie_npiv_quantile(tm, b, tau = 0), "must lie in")
})

test_that("morie_npiv_operator matches Python and shows the decay", {
  t <- seq(0, 6, length.out = 150)
  op <- morie_npiv_operator(sin(t), cos(t), K = 4)
  expect_equal(op$singular_values,
               c(1.468046289451399, 0.015404494477755994,
                 0.0018998712366848785, 8.560089557885301e-09),
               tolerance = 1e-7)
  expect_equal(op$decay_ratio, 5.830939813951071e-09, tolerance = 1e-6)
  expect_equal(op$severity, "severe")
  expect_error(morie_npiv_operator(1:5, 1:4), "same length")
})

test_that("morie_instrument_check matches Python and refuses to test exogeneity", {
  z <- seq(-1, 1, length.out = 120)
  xe <- z * 0.8 + seq(0, 1, length.out = 120)^2
  ic <- morie_instrument_check(xe, z, U = seq(-0.5, 0.5, length.out = 120))
  expect_equal(ic$first_stage_r2, 0.9900739243039508, tolerance = 1e-10)
  expect_equal(ic$first_stage_F, 11769.880327868816, tolerance = 1e-8)
  expect_equal(ic$corr_U_Z, 1, tolerance = 1e-10)
  expect_true(ic$relevant)
  expect_false(ic$exogeneity_testable)
  expect_error(morie_instrument_check(xe, z, U = 1:3), "one entry per row")
})
