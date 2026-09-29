# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/qrntcq_native.R (Ashcroft et al. 2021 quarantine
# efficacy). The generation-time density is recomputed with dgamma,
# every mass by the trapezoid rule on the linear interpolant (approx),
# and the efficacy / utility identities from eqs. (1), (2) and (4).

.qc_grid <- seq(0, 30, by = 0.05)
.qc_dens <- function(shape = 2.83, scale = 1.86, ts = .qc_grid) {
  d <- dgamma(ts, shape = shape, scale = scale)
  d / sum(diff(ts) * (head(d, -1) + tail(d, -1)) / 2)
}
.qc_mass <- function(lo, hi, ts = .qc_grid, ys = .qc_dens()) {
  if (hi <= lo) return(0)
  pts <- c(lo, ts[ts > lo & ts < hi], hi)
  y <- stats::approx(ts, ys, pts)$y
  sum(diff(pts) * (head(y, -1) + tail(y, -1)) / 2)
}

test_that("gamma_generation_time is the trapezoid-normalised gamma density", {
  for (fn in list(morie_qrntcq_gamma_generation_time, morie_qrntcq)) {
    g <- fn(grid = .qc_grid)
    expect_equal(g$t, .qc_grid)
    expect_equal(g$density, .qc_dens(), tolerance = 1e-12)
    g2 <- fn(shape = 4, scale = 0.5, t.max = 10, n = 101L)
    expect_equal(g2$density, .qc_dens(4, 0.5, seq(0, 10, length.out = 101)), tolerance = 1e-12)
    expect_error(fn(shape = 0), "positive")
    expect_error(fn(grid = c(-2, -1)), "integrates to zero")
  }
  g3 <- gamma_generation_time(shape = 4, scale = 0.5, t_max = 10, n = 101L)
  expect_equal(g3$density, .qc_dens(4, 0.5, seq(0, 10, length.out = 101)), tolerance = 1e-12)
  expect_error(gamma_generation_time(scale = -1), "positive")
})

test_that("quarantine_efficacy is prevented mass over remaining mass (eq. 1)", {
  g <- list(t = .qc_grid, density = .qc_dens())
  for (tq in c(2, 3.07)) for (tr in c(5, 10.33)) {
    exp_eff <- .qc_mass(tq, tr) / .qc_mass(tq, 30)
    a <- morie_qrntcq_quarantine_efficacy(tq, tr, g)
    b <- quarantine_efficacy(tq, tr, g)
    expect_equal(a$efficacy, exp_eff, tolerance = 1e-12)
    expect_equal(b$efficacy, exp_eff, tolerance = 1e-12)
    expect_equal(a$pre.quarantine.mass, .qc_mass(0, tq), tolerance = 1e-12)
    expect_equal(b$pre_quarantine_mass, .qc_mass(0, tq), tolerance = 1e-12)
    expect_equal(b$prevented_mass, .qc_mass(tq, tr), tolerance = 1e-12)
  }
  # ceiling: releasing at the end of support prevents everything left
  expect_equal(quarantine_efficacy(3, 30, g)$efficacy, 1, tolerance = 1e-12)
  expect_equal(quarantineefficacy(3, 30, g)$efficacy, 1, tolerance = 1e-12)
  expect_equal(morie_qrntcq_quarantine_efficacy(30, 30, g)$efficacy, 0)
  expect_equal(quarantine_efficacy(30, 30, g)$efficacy, 0)
  # the default density is the 3001-point grid on [0, 30]
  gd <- morie_qrntcq_gamma_generation_time()
  expect_equal(quarantine_efficacy(3, 8)$efficacy,
               .qc_mass(3, 8, gd$t, gd$density) / .qc_mass(3, 30, gd$t, gd$density),
               tolerance = 1e-12)
  expect_error(morie_qrntcq_quarantine_efficacy(5, 4, g), "precedes quarantine start")
  expect_error(morie_qrntcq_quarantine_efficacy(1, 4, g, t.E = 2), "before exposure")
  expect_error(quarantine_efficacy(5, 4, g), "release at 4 precedes quarantine start at 5")
  expect_error(quarantine_efficacy(1, 4, g, t_E = 2), "t_Q 1 < t_E 2", fixed = TRUE)
})

test_that("efficacy_test_and_release mixes detained and released efficacy (eq. 2)", {
  g <- list(t = .qc_grid, density = .qc_dens())
  rel <- .qc_mass(3, 7) / .qc_mass(3, 30)
  det <- .qc_mass(3, 14) / .qc_mass(3, 30)
  a <- morie_qrntcq_efficacy_test_and_release(3, 6, 7, 0.2, g, t.R.positive = 14)
  b <- efficacy_test_and_release(3, 6, 7, 0.2, g, t_R_positive = 14)
  expect_equal(a$efficacy, 0.8 * det + 0.2 * rel, tolerance = 1e-12)
  expect_equal(b$efficacy, 0.8 * det + 0.2 * rel, tolerance = 1e-12)
  expect_equal(b$efficacy_released, rel, tolerance = 1e-12)
  expect_lte(a$efficacy, a$bound)
  full <- testandrelease(3, 6, 7, 0, g)
  expect_equal(full$efficacy, 1, tolerance = 1e-12)
  expect_equal(morie_qrntcq_efficacy_test_and_release(3, 6, 7, 1, g)$efficacy, rel, tolerance = 1e-12)
  expect_error(morie_qrntcq_efficacy_test_and_release(3, 6, 7, 1.5, g), "false-negative")
  expect_error(morie_qrntcq_efficacy_test_and_release(3, 2, 7, 0.1, g), "test cannot precede")
  expect_error(morie_qrntcq_efficacy_test_and_release(3, 6, 5, 0.1, g), "release cannot precede")
  expect_error(efficacy_test_and_release(3, 6, 7, -0.1, g), "got -0.1", fixed = TRUE)
  expect_error(efficacy_test_and_release(3, 2, 7, 0.1, g), "test cannot precede")
  expect_error(efficacy_test_and_release(3, 6, 5, 0.1, g), "release cannot precede")
})

test_that("utility, relative_utility and optimal_duration follow eq. (4)", {
  expect_equal(morie_qrntcq_utility(0.6, 4), 0.15)
  expect_equal(utility(0.6, 4), 0.15)
  expect_error(morie_qrntcq_utility(0.6, 0), "positive")
  expect_error(utility(0.6, -1), "positive")
  g <- list(t = .qc_grid, density = .qc_dens())
  ea <- .qc_mass(3, 8) / .qc_mass(3, 30)
  eb <- .qc_mass(3, 13) / .qc_mass(3, 30)
  for (r in list(morie_qrntcq_relative_utility(8, 13, 3, g),
                 relative_utility(8, 13, 3, g))) {
    expect_equal(r[[1]], (ea / 5) / (eb / 10), tolerance = 1e-12)
  }
  # the infected fraction is accepted and cancels
  expect_equal(relative_utility(8, 13, 3, g, infected_fraction = 0.05)$relative_utility,
               (ea / 5) / (eb / 10), tolerance = 1e-12)
  expect_error(morie_qrntcq_relative_utility(3, 13, 3, g), "positive duration")
  expect_error(relative_utility(8, 2, 3, g), "precedes|positive duration")
  steps <- seq(3.5, 12, by = 0.5)
  u <- vapply(steps, function(t) .qc_mass(3, t) / .qc_mass(3, 30) / (t - 3), 0)
  for (o in list(morie_qrntcq_optimal_duration(3, g, t.max = 12, step = 0.5),
                 optimal_duration(3, g, t_max = 12, step = 0.5))) {
    expect_equal(o$estimate, steps[which.max(u)], tolerance = 1e-12)
    expect_equal(o[[3]], .qc_mass(3, o$estimate) / .qc_mass(3, 30), tolerance = 1e-12)
    expect_equal(o[[4]], max(u), tolerance = 1e-12)
    expect_length(o$curve, length(steps))
  }
})

test_that("qrntcq_cheatsheet states the ceiling and the cancellation", {
  s <- qrntcq_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "CEILING")
  expect_match(s, "infected fraction cancels")
})
