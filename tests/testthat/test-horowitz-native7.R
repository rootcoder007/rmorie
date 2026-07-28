# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the panel-deconvolution mirrors. Anchors
# printed from the Python modules at full double precision --
# testthat tolerances are RELATIVE, so a rounded anchor silently
# weakens the test. The fixture is a linear congruential generator
# written out explicitly so both languages see the same bits.

hrz7_fixture <- function() {
  n <- 120L
  tt <- 3L
  m <- n * tt * 3L + n
  u <- numeric(m)
  s <- 999
  for (k in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[k] <- (s + 0.5) / 4294967296
  }
  z <- stats::qnorm(u)
  x <- array(0, dim = c(n, tt, 2L))
  # numpy fills (n, T, 2) in C order: index = ((i*T)+t)*2 + j
  for (i in seq_len(n)) {
    for (tq in seq_len(tt)) {
      base <- (((i - 1L) * tt) + (tq - 1L)) * 2L
      x[i, tq, 1L] <- z[base + 1L]
      x[i, tq, 2L] <- z[base + 2L]
    }
  }
  uu <- z[(n * tt * 2L + 1L):(n * tt * 2L + n)]
  e <- matrix(z[(n * tt * 2L + n + 1L):m], nrow = n, byrow = TRUE) * 0.5
  beta <- c(1, -0.5)
  xb <- matrix(0, n, tt)
  for (j in 1:2) xb <- xb + x[, , j] * beta[j]
  y <- xb + matrix(uu, n, tt) + e
  list(y = y, x = x, beta = beta, n = n, T = tt)
}

test_that("morie_panel_deconvolution matches morie.fn.hrzpanel", {
  f <- hrz7_fixture()
  o <- morie_panel_deconvolution(f$y, f$x, f$beta)
  expect_equal(o$f_U[31], 0.3582692222898366, tolerance = 1e-9)
  expect_equal(o$f_eps[31], 0.5762129499125729, tolerance = 1e-9)
  expect_equal(o$nu_U, 0.4570313390305189, tolerance = 1e-12)
  expect_equal(o$grid_u[1], -1.9008189495152203, tolerance = 1e-10)
  expect_equal(o$grid_z[61], 1.098132405583493, tolerance = 1e-10)
  expect_true(o$symmetry_required)
  expect_true(o$psi_eps_from_root)
  expect_equal(o$asymptotics_in, "n with T fixed")
  expect_equal(o$T, 3L)
})

test_that("the differenced residual removes the individual effect", {
  f <- hrz7_fixture()
  r1 <- .morie_hrz_panel_residuals(f$y, f$x, f$beta)
  # an enormous extra individual effect must not touch eta at all
  y2 <- f$y + matrix(seq_len(f$n) * 100, f$n, f$T)
  r2 <- .morie_hrz_panel_residuals(y2, f$x, f$beta)
  expect_equal(r1$eta, r2$eta)
  expect_false(isTRUE(all.equal(r1$W, r2$W)))
})

test_that("morie_smoothed_fU matches morie.fn.hrzfnu", {
  f <- hrz7_fixture()
  o <- morie_smoothed_fU(f$y, f$x, f$beta, nu_U = 0.5)
  expect_equal(o$f_U[31], 0.3609557223923062, tolerance = 1e-9)
  expect_equal(o$cutoff, 2)
  expect_equal(o$grid[1], -1.9008189495152203, tolerance = 1e-10)
  expect_true(o$regularisation_required)
  expect_error(morie_smoothed_fU(f$y, f$x, f$beta, nu_U = -1), "positive")
})

test_that("the two density estimators carry separate bandwidths", {
  f <- hrz7_fixture()
  o <- morie_panel_densities(f$y, f$x, f$beta, nu_U = 0.4, nu_eps = 0.9)
  expect_equal(o$f_U[31], 0.3461918188617971, tolerance = 1e-9)
  expect_equal(o$f_eps[31], 0.3326421404115544, tolerance = 1e-9)
  # only f_U divides by |psi_eta|^{1/2}
  expect_true(o$f_U_requires_division)
  expect_false(o$f_eps_requires_division)
  other <- morie_panel_densities(f$y, f$x, f$beta, nu_U = 0.4, nu_eps = 0.5)
  expect_equal(o$f_U, other$f_U)                      # nu_eps left f_U alone
  expect_false(isTRUE(all.equal(o$f_eps, other$f_eps)))
})

test_that("morie_first_passage_time matches Python and falls with the horizon", {
  gu <- seq(-5, 5, length.out = 201)
  gz <- seq(-5, 5, length.out = 201)
  fu <- stats::dnorm(gu)
  fe <- stats::dnorm(gz, sd = 0.5)
  b <- c(1, -0.5)
  o <- morie_first_passage_time(4, 0, 1, matrix(0, 4, 2), b, fu, gu, fe, gz)
  expect_equal(o$probability, 0.8389555548570627, tolerance = 1e-9)
  expect_equal(o$f_W_at_initial, 0.3568248232305543, tolerance = 1e-9)
  expect_true(o$periods_conditionally_independent)
  expect_false(o$periods_marginally_independent)
  p <- vapply(c(2, 4, 8), function(th)
    morie_first_passage_time(th, 0, 1, matrix(0, 8, 2), b, fu, gu,
                             fe, gz)$probability, numeric(1))
  expect_true(all(diff(p) < 0))          # surviving longer is never likelier
  expect_true(all(p >= 0 & p <= 1))
})

test_that("first passage exceeds the naive independent product", {
  # Given U the periods are independent; unconditionally they are
  # not, because they share U. Multiplying marginal probabilities
  # therefore understates survival.
  gu <- seq(-6, 6, length.out = 301)
  gz <- seq(-6, 6, length.out = 301)
  fu <- stats::dnorm(gu, sd = 1.5)
  fe <- stats::dnorm(gz, sd = 0.5)
  b <- c(1, -0.5)
  o <- morie_first_passage_time(6, 0, 1, matrix(0, 6, 2), b, fu, gu, fe, gz)
  marg <- stats::pnorm(1, 0, sqrt(1.5^2 + 0.5^2))
  expect_gt(o$probability, marg^5)
})

test_that("the panel mirrors validate their inputs", {
  f <- hrz7_fixture()
  expect_error(morie_panel_deconvolution(f$y, f$x, 1), "1 entries for 2")
  expect_error(morie_panel_deconvolution(f$y[, 1, drop = FALSE],
                                         f$x[, 1, , drop = FALSE], f$beta),
               "at least 2 periods")
  expect_error(morie_panel_deconvolution(f$y[1:5, ], f$x[1:5, , ], f$beta),
               "at least 10 individuals")
  gu <- seq(-4, 4, length.out = 101)
  expect_error(morie_first_passage_time(1, 0, 1, matrix(0, 3, 2), f$beta,
                                        stats::dnorm(gu), gu,
                                        stats::dnorm(gu), gu),
               "at least 2")
  expect_error(morie_first_passage_time(9, 0, 1, matrix(0, 3, 2), f$beta,
                                        stats::dnorm(gu), gu,
                                        stats::dnorm(gu), gu),
               "3 periods but theta")
  expect_error(morie_first_passage_time(3, 0, 1, matrix(0, 3, 2), f$beta,
                                        -stats::dnorm(gu), gu,
                                        stats::dnorm(gu), gu),
               "non-negative")
})
