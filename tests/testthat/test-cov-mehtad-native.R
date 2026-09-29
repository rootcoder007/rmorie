# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/mehtad_native.R (Mehrotra 1992 predictor-corrector
# interior point). Residuals, the fraction-to-boundary step, the
# centring rule and the Newton system are recomputed directly; the LP
# is checked against its known vertex optimum and strong duality.

.mh_A <- rbind(c(1, 1, 1, 0), c(1, 3, 0, 1))
.mh_b <- c(4, 6)
.mh_c <- c(-1, -2, 0, 0)

test_that("residuals are Ax - b, A'y + s - c and mu = x's / n", {
  x <- c(1, 2, 1, 0.5)
  y <- c(-0.5, 0.25)
  s <- c(0.3, 0.1, 0.2, 0.4)
  r <- mehtad_residuals(.mh_A, .mh_b, .mh_c, x, y, s)
  expect_equal(r$primal, as.numeric(.mh_A %*% x - .mh_b), tolerance = 1e-12)
  expect_equal(r$dual, as.numeric(t(.mh_A) %*% y + s - .mh_c), tolerance = 1e-12)
  expect_equal(r$mu, sum(x * s) / 4, tolerance = 1e-12)
  expect_equal(r$primal_norm, sqrt(sum(r$primal^2)), tolerance = 1e-12)
  expect_equal(r$dual_norm, sqrt(sum(r$dual^2)), tolerance = 1e-12)
})

test_that("max_step is eta times the fraction to the boundary", {
  expect_equal(max_step(c(2, 1), c(-1, -4), 1), 0.25)
  expect_equal(max_step(c(2, 1), c(-1, -4), 0.5), 0.125)
  expect_equal(max_step(c(2, 1), c(1, 3)), 0.9995)
  expect_equal(max_step(c(4, 4), c(-1, -1), 0.9995), 0.9995)
})

test_that("centering_parameter is (mu_affine / mu)^nu", {
  r <- centering_parameter(0.5, 0.1, nu = 3)
  expect_equal(r$sigma, 0.2^3, tolerance = 1e-12)
  expect_equal(r$ratio, 0.2)
  expect_identical(r$approximation, "good")
  expect_identical(centering_parameter(0.5, 0.4)$approximation, "poor")
  expect_equal(centering_parameter(2, 0, nu = 2)$sigma, 0)
  expect_error(centering_parameter(0, 0.1), "mu must be positive")
  expect_error(centering_parameter(1, -0.1), "cannot be negative")
  expect_error(centering_parameter(1, 0.1, nu = 7), "outside the range")
})

test_that("newton_direction solves the three Newton equations", {
  x <- c(1, 2, 1, 0.5)
  y <- c(-0.5, 0.25)
  s <- c(0.3, 0.1, 0.2, 0.4)
  r <- mehtad_residuals(.mh_A, .mh_b, .mh_c, x, y, s)
  rc <- x * s
  for (fn in list(newton_direction, .mehtad_newton)) {
    d <- fn(.mh_A, x, s, r$primal, r$dual, rc)
    expect_equal(as.numeric(.mh_A %*% d$dx), -r$primal, tolerance = 1e-8)
    expect_equal(as.numeric(t(.mh_A) %*% d$dy + d$ds), -r$dual, tolerance = 1e-9)
    expect_equal(s * d$dx + x * d$ds, -rc, tolerance = 1e-9)
  }
})

test_that("solve_lp reaches the vertex optimum with zero duality gap", {
  # max x1 + 2 x2 s.t. x1 + x2 <= 4, x1 + 3 x2 <= 6 -> (3, 1), value -5
  for (fn in list(solve_lp, predictor_corrector, mehrotras_predictor)) {
    r <- fn(.mh_A, .mh_b, .mh_c, tol = 1e-10)
    expect_true(r$converged)
    expect_equal(r$x, c(3, 1, 0, 0), tolerance = 1e-6)
    expect_equal(r$objective, -5, tolerance = 1e-8)
    expect_equal(r$dual_objective, r$objective, tolerance = 1e-7)
    expect_lt(r$primal_residual, 1e-9)
    expect_lt(r$mu, 1e-10)
    expect_true(all(r$x >= 0))
  }
  # without the second-order corrector the same optimum needs more iterations
  a <- solve_lp(.mh_A, .mh_b, .mh_c, tol = 1e-10)
  b <- solve_lp(.mh_A, .mh_b, .mh_c, tol = 1e-10, corrector = FALSE)
  expect_equal(b$objective, -5, tolerance = 1e-8)
  expect_lte(a$iterations, b$iterations)
  expect_false(b$corrector)
  expect_error(solve_lp(.mh_A, c(1, 2, 3), .mh_c), "A is 2x4 but b has 3")
})

test_that("morie_mehtad dispatches every op", {
  x <- c(1, 2, 1, 0.5)
  expect_equal(morie_mehtad("residuals", .mh_A, .mh_b, .mh_c, x, c(0, 0), rep(1, 4))$mu, sum(x) / 4)
  expect_equal(morie_mehtad("max_step", c(2, 1), c(-1, -4), 1)$max_step, 0.25)
  expect_equal(morie_mehtad("centering_parameter", 0.5, 0.1, 3)$sigma, 0.008, tolerance = 1e-12)
  expect_equal(morie_mehtad("solve_lp", .mh_A, .mh_b, .mh_c)$objective, -5, tolerance = 1e-7)
  expect_equal(morie_mehtad("predictor_corrector", .mh_A, .mh_b, .mh_c)$objective, -5, tolerance = 1e-7)
  expect_match(morie_mehtad("cheatsheet")$cheatsheet, "FRACTION-TO-BOUNDARY", fixed = TRUE)
  expect_error(morie_mehtad("simplex"), "unknown op")
  expect_error(morie_mehtad(), "op must be one of")
})
